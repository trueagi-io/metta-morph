import subprocess
import readline
import sys
import os

if len(sys.argv) < 2 or (sys.argv[1] != "compiled" and sys.argv[1] != "interpreted"):
    print('Error: "compiled" or "interpreted" flag expected!')
    exit(1)

#1. Copy mettamorph working instances to temporary folders
def cloneMettamorph(targetFolder):
    os.system("mkdir " + targetFolder)
    os.system("cp ../* " + targetFolder)
CODE_WITH_INJECTED_REPL = "../temp/CODE_WITH_INJECTED_REPL/"
USER_INPUT_INTERPRETER = "../temp/USER_INPUT_INTERPRETER/"
if not os.path.exists(CODE_WITH_INJECTED_REPL):
    cloneMettamorph(CODE_WITH_INJECTED_REPL)
if not os.path.exists(USER_INPUT_INTERPRETER):
    cloneMettamorph(USER_INPUT_INTERPRETER)
workdir = os.getcwd()

#2. inject a REPL into the already built RUN.scm file:
with open("../RUN.scm") as file:
    allcode = file.read()
    allcode += """
    (define (repl)
      (display "Welcome to the Scheme REPL. Press Ctrl+C to exit.\n")
      (let loop ()
        (display "metta> ")
        (flush-output)
        (let ((input (read)))
          (let ((result (eval input)))
            (display result)
            (newline)
            (loop)))))
    (repl)
    """
with open(CODE_WITH_INJECTED_REPL + "RUN.scm", "r") as file:
    prevcode = file.read()
with open(CODE_WITH_INJECTED_REPL + "RUN.scm", "w") as file:
    file.write(allcode)

#3. Compile if compilation requested
if prevcode != allcode and sys.argv[1] == "compiled":
    os.chdir(CODE_WITH_INJECTED_REPL)
    os.system("sh compile_scheme.sh")
    os.system(workdir)

#4. Spawn Scheme interpreter or compile binary on the file with the injected repl:
if sys.argv[1] == "compiled":
    proc = subprocess.Popen([CODE_WITH_INJECTED_REPL + "RUN"], stdin=subprocess.PIPE, stdout=sys.stdout, stderr=sys.stdout, bufsize=1, universal_newlines=True, shell=True)
    proc.stdin.write("(import amb)\n")
if sys.argv[1] == "interpreted":
    proc = subprocess.Popen(["csi -s " + CODE_WITH_INJECTED_REPL + "RUN.scm"], stdin=subprocess.PIPE, stdout=sys.stdout, stderr=sys.stdout, bufsize=1, universal_newlines=True, shell=True)
    proc.stdin.write("(import amb)\n")

#5. use run_scheme.scm to perform metta -> scheme translation of the interactive input line
#   and feed it to the Scheme interpreter or Scheme-compiled subprocess
class REPL:
    def main_loop(self):
        while True:
            try:
                # Use the input function to get user input                
                line = input("metta> ")
                # If the line isn't empty, evaluate it
                if line:
                    command = ""
                    os.system("cp ../temp/CODE.metta " + USER_INPUT_INTERPRETER)
                    with open(USER_INPUT_INTERPRETER + "CODE.metta", "a") as file:
                        file.write(line)
                    try:
                        os.chdir(USER_INPUT_INTERPRETER)
                        os.system("sh run_scheme.sh CODE.metta cat-only")
                        with open("RUN.scm") as file:
                            command = file.read().split("\n")[-3]
                    except:
                        print("Error in metta->scheme translator REPL loop!!")
                    os.chdir(workdir)
                    if command != "":
                        #there is no macro expansion at runtime, so for now we assume the first arg is a function and the rest data
                        command="(print-helper (amb-collect " + command.replace("(!", "[").replace("(quote", "{").replace("(","(list ").replace("[","(").replace("{","(") + "))"
                        proc.stdin.write(command + "\n")
            # Handle Ctrl+C to exit
            except KeyboardInterrupt:
                print("\nCtrl-C Exiting...")
                sys.exit(3)
            # Handle Ctrl+D to exit
            except EOFError:
                print("\n Ctrl-D EOF...")
                sys.exit(0)
            # If there's an error, print it
            except Exception as e:
                print(f"Error: {e}")

#Create Repl with history
if __name__ == "__main__":
    repl = REPL()
    readline.add_history("!(AddBeliefEvent (((ExtSet garfield) --> cat) (1.0 0.9)))")
    readline.add_history("!(AddBeliefEvent (((cat x sky) --> like) (1.0 0.9)))")
    readline.add_history("!(AddBeliefEvent ((sky --> (IntSet blue)) (1.0 0.9)))")
    readline.add_history("!(EternalQuestion (((ExtSet garfield) x (IntSet blue)) --> like))")
    repl.main_loop()
