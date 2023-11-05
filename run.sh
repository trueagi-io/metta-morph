(cd ./temp && cd ..) || mkdir temp
time sh runmetta.sh $1 > ./temp/OUTPUT_METTA
cat ./temp/OUTPUT_METTA
time sh runscheme.sh $1 > ./temp/OUTPUT_SCHEME
cat ./temp/OUTPUT_SCHEME
(cmp ./temp/OUTPUT_METTA ./temp/OUTPUT_SCHEME > /dev/null && echo "==") || echo "!="
