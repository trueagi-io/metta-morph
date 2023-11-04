#1. We load the file within we want to auto-quote all symbols within the Scheme code from the user's MeTTa code
#   which do not refer to functions, macros, MeTTa variables, types, or boolean/number/string literals:
with open("RUN.scm") as file:
    allcode = file.read()
basecode, usercode = allcode.split(";__METTACODE__:")
usercode_nocomments = "\n".join([line.split(";")[0] for line in usercode.split("\n")])

#2. Starting from a list of builtin functions we automatically add the defined MeTTa and Scheme functions&macros:
functions = set(["car", "cdr",              #list functions
                 "-", "+", "*", "/",        #arithmetic functions
                 "and", "or", "not",        #logical functions
                 "min", "max", "abs",       #math functions
                 "<", ">", "==",            #comparison functions
                 "flonum-print-precision"]) #utility functions
for line in allcode.split("\n"):
    names = []
    if line.startswith("(= ("): #MeTTa functions
        names = [line.split("(= (")[1].split(" ")[0].split(")")[0]]
    elif line.startswith("(define ("): #Scheme functions
        names = [line.split("(define (")[1].split(" ")[0].split(")")[0]]
    elif line.startswith("(define-syntax "): #Scheme macros
        names = [line.split("(define-syntax ")[1].strip()]
    elif "(syntax-rules (" in line: #Scheme macro keywords
        names = line.split("(syntax-rules (")[1].split(")")[0].split(" ")
    for name in names:
        functions.add(name)

#3. We now check for yet unquoted symbols:
identified_symbols = set([x for x in usercode_nocomments.replace("(", " ").replace(")"," ").replace("\n","").split(" ") \
                          if x != "" and x[0] != "$" #not a variable
                                     and not x in functions #not a function or macro
                                     and not x.replace("-","").replace(".","").isnumeric() #not a number
                                     and x != "#f" and x != "#t" #not a boolean
                                     and x[0] != '"']) #not a string

#4. We quote the identified symbols in the user code:
newcode = usercode
for x in identified_symbols:
    possible_prefix = [" ", "(", "\n"]
    possible_postfix = [")", " ", "\n"]
    for prefix in possible_prefix:
        for postfix in possible_postfix:
            newcode=newcode.replace(prefix + x + postfix, prefix + "'" + x + postfix)

#5. Additionally, we remove quotes in type definitions, which is easier than keeping track of all defined types:
newcodefinal = ""
for line in newcode.split("\n"):
    if line.startswith("(Typedef "):
       line = line.replace("'", "")
    newcodefinal += line + "\n"

#6. Now, override the file with the one which has the properly quoted symbols
with open("RUN.scm", "w") as file:
    file.write(basecode + "\n;__METTACODE__:" + newcodefinal.replace("''","'"))

