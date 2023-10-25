awk '{gsub(/\(\, /, "(MatchChain ") gsub(/self \(= \(/, "self (\047=def (\047") gsub(/\(: /, "(Typedef ") gsub(/True/, "#t") gsub(/False/, "#f") gsub(/\|\-/, "DERIVE") gsub(/\(\)/, "\047()") gsub(/\(match /, "(Match ") gsub(/\(let /, "(Let ") gsub(/\(let\* /, "(Let\* ") gsub(/\(match /, "(Match ") gsub(/\(case /, "(Case ") gsub(/\(car-atom /, "(car ") gsub(/\(cdr-atom /, "(cdr ")}1' $1 > TRANS.scm 2> /dev/null
rm "PROGRAM.scm"; rm "INPUT.scm"; echo "" > "PROGRAM.scm" ; echo "" > "INPUT.scm"
awk '/;<EVALUATIONS>/ {f=1; next} {if(f) print > "INPUT.scm"; else print > "PROGRAM.scm"}' TRANS.scm
cat mettamorph.scm PROGRAM.scm > RUN.scm
echo "" >> RUN.scm
echo "(! " >> RUN.scm
cat INPUT.scm >> RUN.scm
echo ")" >> RUN.scm
csi -s RUN.scm
