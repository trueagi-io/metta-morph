awk '{gsub(/True/, "#t") gsub(/False/, "#f") gsub(/\|\-/, "DERIVE") gsub(/\(\)/, "\047()") gsub(/\(match /, "(MatchMetta ") gsub(/\(let /, "(LetMetta ") gsub(/\(let\* /, "(Let\*Metta ") gsub(/\(case /, "(CaseMetta ") gsub(/\(car-atom /, "(car ") gsub(/\(cdr-atom /, "(cdr ")}1' $1 > TRANS.scm
awk '/;<EVALUATIONS>/ {f=1; next} {if(f) print > "INPUT.scm"; else print > "PROGRAM.scm"}' TRANS.scm
cat mettamorph.scm PROGRAM.scm > RUN.scm
echo "\n(! " >> RUN.scm
awk '{gsub(/Nil/, "\047()")}1' INPUT.scm > INPUT2.scm
cat INPUT2.scm >> RUN.scm
echo ")" >> RUN.scm
csi -s RUN.scm
