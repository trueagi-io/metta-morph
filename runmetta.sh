awk '{gsub(/\(list /, "(") gsub(/\047/, "")}1' $1 > PROGRAM.metta
cat mettamorph.metta PROGRAM.metta > RUN.metta
metta RUN.metta
