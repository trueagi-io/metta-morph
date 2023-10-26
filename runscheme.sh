awk '{gsub("new-space", "new-space ()") gsub(/\(\, /, "(MatchChain ") gsub(/self \(= \(/, "self (\047=def (\047") gsub(/\(: /, "(Typedef ") gsub(/True/, "#t") gsub(/False/, "#f") gsub(/\|\-/, "DERIVE") gsub(/\(\)/, "\047()") gsub(/\(match /, "(Match ") gsub(/\(let /, "(Let ") gsub(/\(let\* /, "(Let\* ") gsub(/\(match /, "(Match ") gsub(/\(case /, "(Case ") gsub(/\(car-atom /, "(car ") gsub(/\(cdr-atom /, "(cdr ") gsub(/\!\(/, "(! ", $0)}1' $1 > PROGRAM.scm 2> /dev/null
cat mettamorph.scm PROGRAM.scm > RUN.scm
csi -s RUN.scm
