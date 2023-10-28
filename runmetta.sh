awk '{gsub(/\(list /, "(") gsub(/\047/, ""); if($0 ~ /^[(]/ && !($0 ~ /^[(][:|=]/)) { gsub(/^[(]/, "!(add-atom \\&self (", $0); $0 = $0 ")" }}1' $1 > PROGRAM.metta
cat mettamorph.metta PROGRAM.metta > RUN.metta
echo "cat-only" > ./temp/PARAM2_SHOULD
echo "$2" > ./temp/PARAM2_IS
cmp -s ./temp/PARAM2_IS ./temp/PARAM2_SHOULD || metta RUN.metta
