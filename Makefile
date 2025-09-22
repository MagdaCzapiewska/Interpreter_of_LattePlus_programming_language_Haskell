.PHONY: all clean

all: interpreter

interpreter: Main.hs Interpreter.hs TypeChecker.hs
	ghc --make Main.hs -o interpreter -package mtl

clean:
	rm -f interpreter *.o *.hi

