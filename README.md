# LattePlus Programming Language Implementation

This project concerns the implementation of the LattePlus programming language, whose grammar is defined in the file `LattePlus.cf`.

## Project Structure

The solution consists of the following files:

- `Main.hs`  
- `Interpreter.hs`  
- `TypeChecker.hs`  
- `AbsLattePlus.hs`  
- `LexLattePlus.hs`  
- `ParLattePlus.hs`  
- `ErrM.hs`  
- `Makefile`  

A typechecker and interpreter for LattePlus have been implemented.  
I used monad transformers, making use of the `Reader`, `State`, `Except`, and `IO` monads.  

- The global state is composed of:  
  - **environment map**: assigns memory locations to identifiers (variables, functions)
  - **memory map**: assigns values to these memory locations

## Building and Running

Running `make` in the directory containing the `Makefile` builds a working interpreter.  

Usage:
./interpreter program
where program is the file containing the program to be interpreted.

Alternatively, you can run:
./run_interpreter.sh

This will first interpret all programs from the bad directory and then those from the good directory.

## Documentation
Description of the language and feature table (Polish, April 9, 2024):
https://docs.google.com/document/d/1cxLYDHzk76J6EgdQGqJLevmRNgVuHImeg3SrPUvZzfA/edit?usp=sharing

Updated language description and feature table (Polish, May 2024; maximum score decreased from 30 to 26 points):
https://docs.google.com/document/d/1WtA1taNXpbfP4tMPchVo4fP3lwY8Df_vFvGLbnKd27o/edit?usp=sharing

## Example Programs

Sample programs can be found in the good and bad subdirectories, demonstrating all possible errors and language constructs.

Good:
1. At least three value types: int, bool, and string
2. Literals, arithmetic, comparisons
3. Variables, assignment
4. Explicit output of values (statement or built-in procedure print)
5. while, if (with and without else, optionally also if _ elif _ else _ endif syntax)
6. Functions or procedures (without nesting), recursion
7. At least two parameter passing methods (by variable / by value / in/out)
9. Identifier shadowing with static binding (local and global variables or nested procedures/functions)
10. Runtime error handling, e.g., division by zero (with a proper message and interpreter termination)
11. Functions taking and returning values of any supported type (not only procedures; may be function-only, like in C)
12. Static typing (always-terminating type checking before program execution)
13. Arbitrary nested function/procedure definitions with correct static identifier binding (like in Pascal)

Bad:
1. Parsing errors (invalid syntax)
2. Redefinition of printInt, printString, printBool, or printEndline
3. Incorrect number of arguments in a function call
4. Incorrect argument types in a function call
5. An argument that should be a variable passed by reference is another expression instead of a single variable
6. Function call with a variable name instead of a function name
7. Call to an unknown function
8. Use of an unknown variable
8. Variable initialization with an incorrect expression type
9. Missing return statement at the end of a function block
10. Type mismatch in assignment (not limited to declaration/initialization)
11. return statement returns a value of the wrong type
12. Condition in if / ifelif ... else is not of type bool
13. Condition in while is not of type bool
14. Function application errors (same as in points 3–5, but at non-top-level calls)
15. Arithmetic negation applied to a non-int
16. Logical negation applied to a non-bool
17. Multiplication / division / addition / subtraction of non-int values
18. Comparison of non-int values
19. Logical operations (or, and) applied to non-bool values
20. Increment / decrement of a non-int
21. Division / modulo by zero
