module TypeChecker where

import qualified Data.Map as Map
import Control.Monad.Reader
import Control.Monad.Except
import AbsLattePlus

data TypeError = TypeError BNFC'Position String deriving Eq

instance Show TypeError where
    show (TypeError pos msg) =
        case pos of
            Just (line, col) -> "Error at line " ++ show line ++ ", column " ++ show col ++ ": " ++ msg
            Nothing -> "Error: " ++ msg

type Check a = ReaderT Env (Except TypeError) a

data Param = AVal | ARef

type Env = Map.Map Ident (Type, [Param])

noPos :: BNFC'Position
noPos = Just (0, 0)

predefinedFunctions :: [(Ident, (Type, [Param]))]
predefinedFunctions =
  [ (Ident "printInt", (Fun noPos (Void noPos) [Int noPos], []))
  , (Ident "printString", (Fun noPos (Void noPos) [Str noPos], []))
  , (Ident "printBool", (Fun noPos (Void noPos) [Bool noPos], []))
  , (Ident "printEndline", (Fun noPos (Void noPos) [], []))
  ]

initialEnv :: Env
initialEnv = Map.fromList predefinedFunctions

checkProgram :: Program -> Either TypeError ()
checkProgram (Program _ topDefs) = runExcept $ runReaderT (checkTopDefs topDefs) initialEnv

checkTopDefs :: [TopDef] -> Check ()
checkTopDefs [] = return ()
checkTopDefs (def:defs) = do
    env <- checkTopDef def
    local (const env) $ checkTopDefs defs

checkTopDef :: TopDef -> Check Env
checkTopDef (VarDef pos typ items) = do
    env <- ask
    foldM (checkItem typ) env items
checkTopDef (FnDef pos typ ident args block) = do
    let argTypes = map (\arg -> case arg of
                                    ArgVal _ t _ -> t
                                    ArgRef _ t _ -> t
                      ) args
        valrefs = map  (\arg -> case arg of
                                    ArgVal _ _ _ -> AVal
                                    ArgRef _ _ _ -> ARef
                      ) args
        funType = Fun pos typ argTypes
    if isPrintFunction ident
        then throwError $ TypeError pos ("Cannot redefine print function: " ++ show ident)
        else do
            local (Map.insert ident (funType, valrefs)) $ withLocalVars args $ checkBlock block (Just typ) True
            currentEnv <- ask
            return $ Map.insert ident (funType, valrefs) currentEnv
checkTopDef (App pos ident args) = do
    expectedType <- asks (Map.lookup ident)
    case expectedType of
        Just (Fun _ returnType argTypes, valrefs) -> do
            providedArgTypes <- mapM checkExpr args
            -- Sprawdzenie, czy liczba argumentów jest poprawna
            when (length argTypes /= length providedArgTypes) $
                throwError $ TypeError pos ("Incorrect number of arguments for function " ++ show ident)
            -- Sprawdzenie, czy typy argumentów są zgodne z oczekiwanymi typami funkcji
            unless (all (uncurry compareTypes) (zip argTypes providedArgTypes)) $
                throwError $ TypeError pos ("Incorrect types for arguments of function " ++ show ident)
            -- Sprawdzenie, czy odpowiednie argumenty są przekazywane przez referencję
            forM_ (zip valrefs args) $ \(paramRefType, arg) ->
                case paramRefType of
                    ARef -> case arg of
                                EVar{} -> return ()
                                _ -> throwError $ TypeError pos ("Expected variable reference for argument of type ARef, but got: " ++ show arg)
                    AVal -> return ()  -- Dla argumentów przekazywanych przez wartość, dowolne wyrażenie jest poprawne
            env <- ask
            return env
        Nothing -> throwError $ TypeError pos ("Function " ++ show ident ++ " not found")
        _ -> throwError $ TypeError pos ("Variable " ++ show ident ++ " is not a function")

checkItem :: Type -> Env -> Item -> Check Env
checkItem typ env (NoInit pos ident) = checkItemInit typ ident pos env
checkItem typ env (Init pos ident expr) = do
    exprType <- checkExpr expr
    if compareTypes typ exprType
        then checkItemInit typ ident pos env
        else throwError $ TypeError pos ("Expected type " ++ show typ ++ " but got " ++ show exprType ++ " for variable " ++ show ident)

checkItemInit :: Type -> Ident -> BNFC'Position -> Env -> Check Env
checkItemInit typ ident _ env = return $ Map.insert ident (typ, []) env

compareTypes :: Type -> Type -> Bool
compareTypes (Int _) (Int _) = True
compareTypes (Str _) (Str _) = True
compareTypes (Bool _) (Bool _) = True
compareTypes (Void _) (Void _) = True
compareTypes (Fun _ retType1 argTypes1) (Fun _ retType2 argTypes2) =
    compareTypes retType1 retType2 && all (uncurry compareTypes) (zip argTypes1 argTypes2)
compareTypes _ _ = False

checkBlock :: Block -> Maybe Type -> Bool -> Check Env
checkBlock (Block _ stmts) returnType lastStmtMustBeReturn = do
    env <- ask
    finalEnv <- checkStmts stmts env
    local (const finalEnv) $ do
        case returnType of
            Just _ ->
                if lastStmtMustBeReturn && (null stmts || not (isReturnStmt (last stmts)))
                    then throwError $ TypeError Nothing "No return statement at the end of the block"
                    else return finalEnv
            Nothing -> return finalEnv
  where
    checkStmts :: [Stmt] -> Env -> Check Env
    checkStmts [] env = return env
    checkStmts (s:ss) env = do
        newEnv <- local (const env) $ checkStmt s returnType
        checkStmts ss newEnv

    isReturnStmt :: Stmt -> Bool
    isReturnStmt stmt = case stmt of
        Ret _ _ -> True
        VRet _ -> True
        _ -> False

checkStmt :: Stmt -> Maybe Type -> Check Env
checkStmt stmt returnType = case stmt of
    Empty _ -> ask
    BStmt _ block -> do
        checkBlock block returnType False
        ask
    VarDecl _ typ items -> do
        env <- ask
        foldM (checkItem typ) env items
    FnDecl pos typ ident args block -> do
        let argTypes = map (\arg -> case arg of
                                     ArgVal _ t ident -> t
                                     ArgRef _ t ident -> t
                          ) args
            valrefs = map (\arg -> case arg of
                                     ArgVal _ _ _ -> AVal
                                     ArgRef _ _ _ -> ARef
                          ) args
            funType = Fun pos typ argTypes
        if isPrintFunction ident
            then throwError $ TypeError pos ("Cannot redefine print function: " ++ show ident)
            else do
                env <- ask
                let newEnv = Map.insert ident (funType, valrefs) env
                local (const newEnv) $ withLocalVars args $ checkBlock block (Just typ) True
                currentEnv <- ask
                return $ Map.insert ident (funType, valrefs) currentEnv
    Ass pos ident expr -> do
        varType <- lookupVarType ident pos
        exprType <- checkExpr expr
        unless (compareTypes varType exprType) $
            throwError $ TypeError pos ("Type mismatch in assignment to variable " ++ show ident ++ ": expected " ++ show varType ++ ", got " ++ show exprType)
        ask
    Incr pos ident -> do
        checkVarInt ident pos
        ask
    Decr pos ident -> do
        checkVarInt ident pos
        ask
    Ret pos expr -> do
        exprType <- checkExpr expr
        case returnType of
            Just t -> unless (compareTypes t exprType) $
                throwError $ TypeError pos ("Incorrect return type: expected " ++ show t ++ ", got " ++ show exprType)
            Nothing -> throwError $ TypeError pos "Return statement outside of a function"
        ask
    VRet pos -> do
        case returnType of
            Just Void{} -> return ()
            Just t -> throwError $ TypeError pos ("Incorrect return type: expected " ++ show t ++ ", got Void")
            Nothing -> throwError $ TypeError pos "Return statement outside of a function"
        ask
    Cond pos cond stmt -> do
        condType <- checkExpr cond
        unless (compareTypes condType (Bool pos)) $
            throwError $ TypeError pos "Condition must be of type Bool"
        checkBlock stmt returnType False
        ask
    CondElse pos cond stmt1 stmt2 -> do
        condType <- checkExpr cond
        unless (compareTypes condType (Bool pos)) $
            throwError $ TypeError pos "Condition must be of type Bool"
        checkBlock stmt1 returnType False
        checkBlock stmt2 returnType False
        ask
    While pos cond stmt -> do
        condType <- checkExpr cond
        unless (compareTypes condType (Bool pos)) $
            throwError $ TypeError pos "Condition must be of type Bool"
        checkBlock stmt returnType False
    SExp _ expr -> do
        void $ checkExpr expr
        ask

checkExpr :: Expr -> Check Type
checkExpr expr = case expr of
    EVar pos ident -> lookupVarType ident pos
    ELitInt pos _ -> return $ Int pos
    ELitTrue pos -> return $ Bool pos
    ELitFalse pos -> return $ Bool pos
    EApp pos ident args -> do
        expectedReturnType <- asks (Map.lookup ident)
        case expectedReturnType of
            Just (Fun _ returnType argTypes, valrefs) -> do
                providedArgTypes <- mapM checkExpr args
                -- Sprawdzenie, czy liczba argumentów jest poprawna
                when (length argTypes /= length providedArgTypes) $
                    throwError $ TypeError pos ("Incorrect number of arguments for function " ++ show ident)
                -- Sprawdzenie, czy typy argumentów są zgodne z oczekiwanymi typami funkcji
                unless (all (uncurry compareTypes) (zip argTypes providedArgTypes)) $
                    throwError $ TypeError pos ("Incorrect types for arguments of function " ++ show ident)
                -- Sprawdzenie, czy odpowiednie argumenty są przekazywane przez referencję
                forM_ (zip valrefs args) $ \(paramRefType, arg) ->
                    case paramRefType of
                        ARef -> case arg of
                                    EVar{} -> return ()
                                    _ -> throwError $ TypeError pos ("Expected variable reference for argument of type ARef, but got: " ++ show arg)
                        AVal -> return ()  -- Dla argumentów przekazywanych przez wartość, dowolne wyrażenie jest poprawne
                return returnType
            Nothing -> throwError $ TypeError pos ("Function " ++ show ident ++ " not found")
            _ -> throwError $ TypeError pos ("Variable " ++ show ident ++ " is not a function")
    EString pos _ -> return $ Str pos
    Neg pos expr -> do
        exprType <- checkExpr expr
        if compareTypes exprType (Int pos)
            then return $ Int pos
            else throwError $ TypeError pos "Negation applied to non-integer"
    Not pos expr -> do
        exprType <- checkExpr expr
        if compareTypes exprType (Bool pos)
            then return $ Bool pos
            else throwError $ TypeError pos "Logical negation applied to non-boolean"
    EMul pos expr1 op expr2 -> do
        exprType1 <- checkExpr expr1
        exprType2 <- checkExpr expr2
        case (exprType1, exprType2) of
            (Int{}, Int{}) -> return $ Int pos
            _ -> throwError $ TypeError pos "Arithmetic operation applied to non-integer"
    EAdd pos expr1 op expr2 -> do
        exprType1 <- checkExpr expr1
        exprType2 <- checkExpr expr2
        case (exprType1, exprType2) of
            (Int{}, Int{}) -> return $ Int pos
            _ -> throwError $ TypeError pos "Arithmetic operation applied to non-integer"
    ERel pos expr1 op expr2 -> do
        exprType1 <- checkExpr expr1
        exprType2 <- checkExpr expr2
        case (exprType1, exprType2) of
            (Int{}, Int{}) -> return $ Bool pos
            _ -> throwError $ TypeError pos "Relational operation applied to non-integer"
    EAnd pos expr1 expr2 -> do
        exprType1 <- checkExpr expr1
        exprType2 <- checkExpr expr2
        if (compareTypes exprType1 (Bool pos)) && (compareTypes exprType2 (Bool pos))
            then return $ Bool pos
            else throwError $ TypeError pos "Logical operation applied to non-boolean"
    EOr pos expr1 expr2 -> do
        exprType1 <- checkExpr expr1
        exprType2 <- checkExpr expr2
        if (compareTypes exprType1 (Bool pos)) && (compareTypes exprType2 (Bool pos))
            then return $ Bool pos
            else throwError $ TypeError pos "Logical operation applied to non-boolean"

lookupVarType :: Ident -> BNFC'Position -> Check Type
lookupVarType ident pos = do
    env <- ask
    case Map.lookup ident env of
        Just (t, _) -> return t
        Nothing -> throwError $ TypeError pos ("Variable " ++ show ident ++ " not found")

withLocalVars :: [Arg] -> Check a -> Check a
withLocalVars args action = do
    env <- ask
    let argBindings = map (\arg -> case arg of
                                     ArgVal _ t ident -> (ident, (t, []))
                                     ArgRef _ t ident -> (ident, (t, []))
                          ) args
    local (Map.union (Map.fromList argBindings)) action

checkVarInt :: Ident -> BNFC'Position -> Check ()
checkVarInt ident pos = do
    varType <- lookupVarType ident pos
    unless (compareTypes varType (Int pos)) $ throwError $ TypeError pos ("Variable " ++ show ident ++ " is not an integer")
    
isPrintFunction :: Ident -> Bool
isPrintFunction (Ident name) = name `elem` ["printInt", "printString", "printBool", "printEndline"]
