**REPL**

After ```sh run_scheme.sh``` and/or ```sh run_metta.sh``` is executed in the main directory, ```RUN.scm``` and ```RUN.metta``` are present there.

This script allows to create an interactive REPL which loads these files and allows additional user input in MeTTa format by simply invoking ```python3 run_metta_repl.py``` or ```python3 run_metta_repl.py interpreted``` / ```python3 run_metta_repl.py compiled```.

For the MeTTa REPL injecting arbitrary MeTTa code is possible at runtime, while the MeTTa-Scheme-translating REPL allows for one MeTTa function call where arguments can be nested expressions of symbols and/or functions to be passed, but not nested function calls at current stage.

