(cd ./temp && cd ..) || mkdir temp
cat mettamorphmetta.metta > ./temp/intermediate.scm
cat $1 >> ./temp/intermediate.scm
cp ./temp/intermediate.scm ./temp/CODE.metta > /dev/null 2>&1 #copy for repl only
awk '{gsub("new-space", "new-space ()") gsub(/\(\, /, "(MatchChain ") gsub(/self \(= \(/, "self (\047=def (\047") gsub(/self \(: /, "self (\047:def \047") gsub(/True/, "#t") gsub(/False/, "#f") gsub(/\|\-/, "DERIVE") gsub(/\(\)/, "\047()") gsub(/\(match /, "(Match ") gsub(/\(let /, "(Let ") gsub(/\(let\* /, "(Let* ") gsub(/\(match /, "(Match ") gsub(/\(case /, "(Case ") gsub(/\(car-atom /, "(car ") gsub(/\(cdr-atom /, "(cdr "); if($0 ~ /^[(]/ && !($0 ~ /^\(define/) && !($0 ~ /^[(][:|=]/)) { gsub(/^[(]/, "!(add-atom \047\\&self (", $0); $0 = $0 ")" } gsub(/\!\(/, "(! ", $0) gsub(/\(: /, "(Typedef ")}1' ./temp/intermediate.scm > ./temp/PROGRAM.scm 2> /dev/null
cat mettamorph.scm > RUN.scm
echo ";__METTACODE__:" >> RUN.scm
cat ./temp/PROGRAM.scm >> RUN.scm
python3 autoquote.py
echo "cat-only" > ./temp/PARAM2_SHOULD
echo "$2" > ./temp/PARAM2_IS
cmp -s ./temp/PARAM2_IS ./temp/PARAM2_SHOULD || csi -s RUN.scm
