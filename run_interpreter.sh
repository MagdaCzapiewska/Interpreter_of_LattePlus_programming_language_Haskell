#!/bin/bash

# Ścieżka do interpretera
INTERPRETER="./interpreter"

# Katalogi z programami
GOOD_DIR="./good"
BAD_DIR="./bad"

# Funkcja do wywoływania interpretera z plikami w danym katalogu
run_interpreter_on_files() {
    local dir=$1
    for file in "$dir"/*; do
        echo "Wywołuję $INTERPRETER z argumentem $file"
        $INTERPRETER "$file"
        echo # Dodanie pustego wiersza
    done
}

# Wywołanie interpretera na plikach w katalogach good i bad
run_interpreter_on_files "$BAD_DIR"
run_interpreter_on_files "$GOOD_DIR"

