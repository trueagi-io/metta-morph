sh runmetta.sh > OUTPUT_METTA
cat OUTPUT_METTA
sh runscheme.sh > OUTPUT_SCHEME
cat OUTPUT_SCHEME
(cmp OUTPUT_METTA OUTPUT_SCHEME > /dev/null && echo "==") || echo "!="
