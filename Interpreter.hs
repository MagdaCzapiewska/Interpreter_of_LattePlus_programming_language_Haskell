module Interpreter where

import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Trans.Class (lift)
import AbsLattePlus

data Value = VInt Integer
           | VString String
           | VBool Bool
           | VVoid
           | VFunc Env [Arg] (Block' BNFC'Position)
  deriving Eq
  
data ReturnValue = RValue Value | RNone
  deriving (Eq, Show)

type Loc = Integer
type Env = Map.Map Ident Loc
type Store = Map.Map Loc Value

data MyError = MyError BNFC'Position String

type Eval a = ReaderT Env (StateT Store (ExceptT MyError IO)) a

instance Show Value where
    show (VInt i) = show i
    show (VString s) = s
    show (VBool b) = show b
    show VVoid = "()"
    show (VFunc _ _ _) = "<function>"
    
instance Show MyError where
    show (MyError pos msg) = "(" ++ show (fst <$> pos) ++ "," ++ show (snd <$> pos) ++ "): " ++ msg
    
throwErrorWithPos :: BNFC'Position -> String -> Eval a
throwErrorWithPos pos msg = throwError $ MyError pos msg

interpret :: Program -> IO (Either MyError ((ReturnValue, Env), Store))
interpret (Program _ topDefs) = evalProgram Map.empty Map.empty topDefs

evalProgram :: Env -> Store -> [TopDef] -> IO (Either MyError ((ReturnValue, Env), Store))
evalProgram env store topDefs = runEval env store (evalTopDefs topDefs)

runEval :: Env -> Store -> Eval a -> IO (Either MyError (a, Store))
runEval env initialStore ev = runExceptT $ runStateT (runReaderT ev env) initialStore

evalTopDefs :: [TopDef] -> Eval (ReturnValue, Env)
evalTopDefs [] = do
    env <- ask
    return (RNone, env)
evalTopDefs (def:defs) = do
    (retValue, env) <- evalTopDef def
    (updatedRetValue, updatedEnv) <- local (const env) $ evalTopDefs defs
    return (updatedRetValue, updatedEnv)

evalTopDef :: TopDef -> Eval (ReturnValue, Env)
evalTopDef (VarDef _ typ items) = do
    env <- ask
    updatedEnv <- foldM (declareItem typ) env items
    return (RNone, updatedEnv)

evalTopDef (FnDef _ _ ident args block) = do
    env <- ask
    let fun = VFunc env args block
    updatedEnv <- allocateAndInsert ident fun env
    return (RNone, updatedEnv)

evalTopDef (App _ ident args) = case ident of
    (Ident "printEndline") -> do
        printEndline
        currentEnv <- ask
        return (RNone, currentEnv)
    (Ident "printString") -> do
        printValue =<< evalExpr (head args)
        currentEnv <- ask
        return (RNone, currentEnv)
    (Ident "printInt") -> do
        printValue =<< evalExpr (head args)
        currentEnv <- ask
        return (RNone, currentEnv)
    (Ident "printBool") -> do
        printValue =<< evalExpr (head args)
        currentEnv <- ask
        return (RNone, currentEnv)
    (Ident _) -> do
        env <- ask
        store <- get
        case Map.lookup ident env of
            Just loc -> case Map.lookup loc store of
                Just (VFunc fEnv fArgs block) -> do
                    argValues <- mapM evalExpr args
                    newEnv <- prepareFuncEnv fEnv ident (VFunc fEnv fArgs block) fArgs args argValues
                    (retVal, _) <- local (const newEnv) $ evalBlock block
                    return (retVal, env)
                _ -> throwErrorWithPos Nothing $ "Function " ++ show ident ++ " not found in store"
            _ -> throwErrorWithPos Nothing $ "Function " ++ show ident ++ " not found in environment"

prepareFuncEnv :: Env -> Ident -> Value -> [Arg] -> [Expr] -> [Value] -> Eval Env
prepareFuncEnv fEnv ident func fArgs args argValues = do
    -- Add the function to the environment for recursive calls
    tempEnv <- allocateAndInsert ident func fEnv
    finalEnv <- foldM addArgToEnv tempEnv (zip3 fArgs args argValues)
    return finalEnv
  where
    addArgToEnv env (ArgVal _ _ ident, _, val) = do
        updatedEnv <- allocateAndInsert ident val env
        return updatedEnv
    addArgToEnv env (ArgRef _ _ ident, argExpr, _) = do
        currentEnv <- ask
        store <- get
        case argExpr of
            EVar _ argIdent -> case Map.lookup argIdent currentEnv of
                Just loc -> return $ Map.insert ident loc env
                Nothing -> throwErrorWithPos Nothing $ "Variable " ++ show argIdent ++ " not found in environment"
            _ -> throwErrorWithPos Nothing "ArgRef must be a variable"

        
declareItem :: Type -> Env -> Item' BNFC'Position -> Eval Env
declareItem typ env item = case item of
    NoInit _ ident -> do
        defaultValue <- getDefaultVal typ
        allocateAndInsert ident defaultValue env
    Init _ ident expr -> do
        value <- evalExpr expr
        allocateAndInsert ident value env

allocateAndInsert :: Ident -> Value -> Env -> Eval Env
allocateAndInsert ident value env = do
    loc <- allocateLoc
    modify (Map.insert loc value)
    return (Map.insert ident loc env)

getDefaultVal :: Type -> Eval Value
getDefaultVal typ = case typ of
    Int _ -> return (VInt 0)
    Bool _ -> return (VBool False)
    Str _ -> return (VString "")
    _ -> throwErrorWithPos Nothing "Unsupported type for default value"

allocateLoc :: Eval Loc
allocateLoc = do
    store <- get
    return $ fromIntegral (Map.size store)

printEndline :: Eval ()
printEndline = liftIO $ putStrLn ""

printValue :: Value -> Eval ()
printValue (VString s) = liftIO $ putStrLn s
printValue (VInt n) = liftIO $ print n
printValue (VBool b) = liftIO $ print b
printValue _ = throwError $ MyError Nothing "Invalid argument type for print function"

evalBlock :: Block -> Eval (ReturnValue, Env)
evalBlock (Block _ stmts) = do
    env <- ask
    evalStmts stmts RNone env

evalStmts :: [Stmt] -> ReturnValue -> Env -> Eval (ReturnValue, Env)
evalStmts [] retValue env = return (retValue, env)
evalStmts (stmt:stmts) retValue env = do
    (newRetValue, newEnv) <- local (const env) $ evalStmt stmt
    case newRetValue of
        RValue _ -> return (newRetValue, newEnv)
        RNone    -> evalStmts stmts newRetValue newEnv

evalStmt :: Stmt -> Eval (ReturnValue, Env)
evalStmt stmt = case stmt of
    Empty _ -> do
        env <- ask
        return (RNone, env)
    BStmt _ block -> do
        (retValue, blockEnv) <- evalBlock block
        env <- ask
        return (retValue, env)  -- Restore the original environment after the block
    VarDecl _ typ items -> do
        env <- ask
        newEnv <- foldM (declareItem typ) env items
        return (RNone, newEnv)
    FnDecl _ _ ident args block -> do
        env <- ask
        let fun = VFunc env args block
        updatedEnv <- allocateAndInsert ident fun env
        return (RNone, updatedEnv)
    Ass _ ident expr -> do
        val <- evalExpr expr
        env <- ask
        updateVar ident val env
        return (RNone, env)
    Incr _ ident -> do
        modifyVar ident (+1)
        env <- ask
        return (RNone, env)
    Decr _ ident -> do
        modifyVar ident (subtract 1)
        env <- ask
        return (RNone, env)
    Ret _ expr -> do
        val <- evalExpr expr
        env <- ask
        return (RValue val, env)
    VRet _ -> do
        env <- ask
        return (RValue VVoid, env)
    Cond pos expr stmt -> do
        VBool cond <- evalExpr expr
        if cond
            then evalBlock stmt
            else do
                env <- ask
                return (RNone, env)
    CondElse pos expr stmt1 stmt2 -> do
        VBool cond <- evalExpr expr
        if cond
            then evalBlock stmt1
            else evalBlock stmt2
    While pos expr stmt -> do
        VBool cond <- evalExpr expr
        if cond
            then do
                (retValue, _) <- evalBlock stmt
                case retValue of
                    RNone -> evalStmt (While pos expr stmt)
                    RValue _ -> do
                        env <- ask
                        return (retValue, env)
            else do
                env <- ask
                return (RNone, env)
    SExp _ expr -> do
        _ <- evalExpr expr
        env <- ask
        return (RNone, env)

updateVar :: Ident -> Value -> Env -> Eval ()
updateVar ident val env = do
    store <- get
    case Map.lookup ident env of
        Just loc -> modify (Map.insert loc val)
        Nothing -> throwErrorWithPos Nothing $ "Variable " ++ show ident ++ " not found"

modifyVar :: Ident -> (Integer -> Integer) -> Eval ()
modifyVar ident f = do
    env <- ask
    store <- get
    case Map.lookup ident env of
        Just loc -> case Map.lookup loc store of
            Just (VInt n) -> modify (Map.insert loc (VInt (f n)))
            _ -> throwErrorWithPos Nothing $ "Variable " ++ show ident ++ " is not an integer"
        Nothing -> throwErrorWithPos Nothing $ "Variable " ++ show ident ++ " not found"

getIdent :: Item' BNFC'Position -> Ident
getIdent (NoInit _ ident) = ident
getIdent (Init _ ident _) = ident

evalExpr :: Expr -> Eval Value
evalExpr expr = case expr of
    EVar _ ident -> do
        env <- ask
        store <- get
        case Map.lookup ident env of
            Just loc -> case Map.lookup loc store of
                Just v -> return v
                _ -> throwError $ MyError Nothing $ "Variable " ++ show ident ++ " not found in store"
            _ -> throwError $ MyError Nothing $ "Variable " ++ show ident ++ " not found in environment"
    ELitInt _ n -> return $ VInt n
    ELitTrue _ -> return $ VBool True
    ELitFalse _ -> return $ VBool False
    EApp _ ident args -> case ident of
        (Ident "printEndline") -> do
            printEndline
            currentEnv <- ask
            return VVoid
        (Ident "printString") -> do
            printValue =<< evalExpr (head args)
            currentEnv <- ask
            return VVoid
        (Ident "printInt") -> do
            printValue =<< evalExpr (head args)
            currentEnv <- ask
            return VVoid
        (Ident "printBool") -> do
            printValue =<< evalExpr (head args)
            currentEnv <- ask
            return VVoid
        (Ident _) -> do
            env <- ask
            store <- get
            case Map.lookup ident env of
                Just loc -> case Map.lookup loc store of
                    Just (VFunc fEnv fArgs block) -> do
                        argValues <- mapM evalExpr args
                        newEnv <- prepareFuncEnv fEnv ident (VFunc fEnv fArgs block) fArgs args argValues
                        (retVal, _) <- local (const newEnv) $ evalBlock block
                        return $ case retVal of
                            RValue val -> val
                            RNone -> VVoid
                    _ -> throwErrorWithPos Nothing $ "Function " ++ show ident ++ " not found in store"
                _ -> throwErrorWithPos Nothing $ "Function " ++ show ident ++ " not found in environment"
    EString _ s -> return $ VString s
    Neg _ expr -> do
        VInt v <- evalExpr expr
        return $ VInt (-v)
    Not _ expr -> do
        VBool v <- evalExpr expr
        return $ VBool (not v)
    EMul _ expr1 op expr2 -> do
        VInt v1 <- evalExpr expr1
        VInt v2 <- evalExpr expr2
        case op of
            Times _ -> return $ VInt (v1 * v2)
            Div _ -> if v2 == 0 then throwError $ MyError Nothing "Division by zero" else return $ VInt (v1 `div` v2)
            Mod _ -> if v2 == 0 then throwError $ MyError Nothing "Modulo by zero" else return $ VInt (v1 `mod` v2)
    EAdd _ expr1 op expr2 -> do
        VInt v1 <- evalExpr expr1
        VInt v2 <- evalExpr expr2
        case op of
            Plus _ -> return $ VInt (v1 + v2)
            Minus _ -> return $ VInt (v1 - v2)
    ERel _ expr1 op expr2 -> do
        VInt v1 <- evalExpr expr1
        VInt v2 <- evalExpr expr2
        case op of
            LTH _ -> return $ VBool (v1 < v2)
            LE _ -> return $ VBool (v1 <= v2)
            GTH _ -> return $ VBool (v1 > v2)
            GE _ -> return $ VBool (v1 >= v2)
            EQU _ -> return $ VBool (v1 == v2)
            NE _ -> return $ VBool (v1 /= v2)
    EAnd _ expr1 expr2 -> do
        VBool v1 <- evalExpr expr1
        if v1 then evalExpr expr2 else return $ VBool False
    EOr _ expr1 expr2 -> do
        VBool v1 <- evalExpr expr1
        if v1 then return $ VBool True else evalExpr expr2
