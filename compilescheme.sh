echo "Compilation started."
Flags="-O5 -strict-types -lfa2"
csc $Flags -DUSE_TYPES RUN.scm || (echo "\nERROR IN TYPE DEFINITIONS ENCOUNTERED!! COMPILING WITHOUT TYPE INFO NOW..." && csc $Flags RUN.scm)
#To c: add -t
echo "Compilation complete."
