echo "Compilation started."
Flags=""
#Uncomment the following if you want to maximize performance if you do not mind waiting for a few minutes to end up with a heavily-optimized binary!
#In the following O5 flags are mentioned with two differences: -strict-types is used which is not in O5 but for Mettamorph is a valid assumption.
#-block is not used as it would not allow to call code from the repl, if that doesn't matter you can add it to the list as well:
#Flags="-strict-types -optimize-leaf-routines -inline -lfa2 -inline-global -specialize -unsafe -disable-interrupts -no-trace -no-lambda-info -clustering"
csc $Flags -DUSE_TYPES RUN.scm || (echo "\nERROR IN TYPE DEFINITIONS ENCOUNTERED!! COMPILING WITHOUT TYPE INFO NOW..." && csc $Flags RUN.scm)
echo "Compilation complete."
