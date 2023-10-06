awk '{gsub(/\(list /, "( ")}1' $1 > TRANS.metta
awk '/;<EVALUATIONS>/ {f=1; next} {if(f) print > "INPUT.metta"; else print > "PROGRAM.metta"}' TRANS.metta
cat mettamorph.metta PROGRAM.metta > RUN.metta
echo "\n!" >> RUN.metta
cat INPUT.metta >> RUN.metta
echo "" >> RUN.metta
metta RUN.metta
