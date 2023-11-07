(cd ./temp && cd ..) || mkdir temp
sh run_metta.sh $1 $2 > ./temp/OUTPUT_METTA
cat ./temp/OUTPUT_METTA
sh run_scheme.sh $1 $2 > ./temp/OUTPUT_SCHEME
cat ./temp/OUTPUT_SCHEME
(cmp ./temp/OUTPUT_METTA ./temp/OUTPUT_SCHEME > /dev/null && echo "==") || echo "!="
