def WithoutComments(code):
    lines = code.split("\n")
    return "\n".join([line if line.startswith(";__METTACODE__:") else line.split(";")[0] for line in lines])

with open("RUN.scm") as file:
    code = WithoutComments(file.read())
lines = code.split("\n")

names = set(["Let*", "Let", "Match", "MatchChain", "If", "Case", "car", "cdr", "-", "+", "*", "/", "and", "or", "not", "trace!", "%Undefined%", "%void%",
             "Expression",  "%Undefined%", "Atom", "Symbol", "Number", "Typedef", "min", "max", "abs", "collapse", "superpose", 
             "do", "sequential", "flonum-print-precision",
             "<", ">", "==", "->", "=", "!", "'=def", "'"])
for line in lines:
    if line.startswith("(= ("):
        name = line.split("(= (")[1].split(" ")[0].split(")")[0]
        names.add(name)
    if line.startswith("(define ("):
        name = line.split("(define (")[1].split(" ")[0].split(")")[0]
        names.add(name)

codeparts = code.split(";__METTACODE__:")
basecode = codeparts[0]
editcode = codeparts[1]

identified_symbols = set([x for x in editcode.replace("(", " ").replace(")"," ").replace("\n","").split(" ") \
                         if x != "" and x[0] != "$" and not x in names and not x.replace("-","").replace(".","").isnumeric() and x!="#f" and x!="#t"])
#print(identified_symbols)
#print(names)

newcode = editcode
for x in identified_symbols:
    possible_prefix = [" ", "(", "\n"]
    possible_postfix = [")", " ", "\n"]
    for prefix in possible_prefix:
        for postfix in possible_postfix:
            newcode=newcode.replace(prefix + x + postfix, prefix + "'" + x + postfix)

newcodefinal = ""
for line in newcode.split("\n"):
    newcodefinal += line + "\n"

with open("RUN.scm", "w") as file:
    file.write(basecode + "\n;__METTACODE__:" + newcodefinal.replace("''","'"))

