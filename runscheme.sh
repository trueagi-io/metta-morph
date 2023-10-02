awk '{gsub(/\(let /, "(LetMetta ") gsub(/\(let\* /, "(Let\*Metta ") gsub(/\(If /, "(if ") gsub(/\(case /, "(CaseMetta ") gsub(/\(\$else /, "(else ")}1' code.metta > TRANS.scm
awk '/;<EVALUATIONS>/ {f=1; next} {if(f) print > "INPUT.scm"; else print > "PROGRAM.scm"}' TRANS.scm
cat mettamorph.scm PROGRAM.scm > RUN.scm
echo "\n(! (list " >> RUN.scm
cat INPUT.scm >> RUN.scm
echo "))" >> RUN.scm
csi -s RUN.scm
