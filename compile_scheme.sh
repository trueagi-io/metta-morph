echo "Compilation started."
#Flags="-O5 -strict-types -lfa2" can cause issues for repl
Flags=""
csc $Flags -DUSE_TYPES RUN.scm || (echo "\nERROR IN TYPE DEFINITIONS ENCOUNTERED!! COMPILING WITHOUT TYPE INFO NOW..." && csc $Flags RUN.scm)
echo "Compilation complete."
