awk '{gsub(/self \(= /, "self (=def ") gsub(/ Nat)/, " integer)") gsub(/ Nat /, " integer ") gsub(/ Bool)/, " boolean)") gsub(/ Bool /, " boolean ") gsub(/ String)/, " string)") gsub(/ String /, " string ") gsub(/ Atom)/, " symbol)") gsub(/ Atom /, " symbol ") gsub(/ Expression)/, " list)") gsub(/ Expression /, " list ") gsub(/ Number)/, " number)") gsub(/ Number /, " number ") gsub(/\(: /, "(Typedef ") gsub(/True/, "#t") gsub(/False/, "#f") gsub(/\|\-/, "DERIVE") gsub(/\(\)/, "\047()") gsub(/\(match /, "(Match ") gsub(/\(let /, "(Let ") gsub(/\(let\* /, "(Let\* ") gsub(/\(match /, "(Match ") gsub(/\(case /, "(Case ") gsub(/\(car-atom /, "(car ") gsub(/\(cdr-atom /, "(cdr ")}1' $1 > TRANS.scm 2> /dev/null
rm "PROGRAM.scm"; rm "INPUT.scm"; echo "" > "PROGRAM.scm" ; echo "" > "INPUT.scm"
awk '/;<EVALUATIONS>/ {f=1; next} {if(f) print > "INPUT.scm"; else print > "PROGRAM.scm"}' TRANS.scm
cat mettamorph.scm PROGRAM.scm > RUN.scm
echo "" >> RUN.scm
echo "(! " >> RUN.scm
cat INPUT.scm >> RUN.scm
echo ")" >> RUN.scm
csi -s RUN.scm
