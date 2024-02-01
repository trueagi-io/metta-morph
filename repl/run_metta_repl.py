#Original author: Douglas Miles
#Edited by Patrick Hammer
from hyperon.runner import MeTTa
import sys
import readline

runner = MeTTa()
class REPL:
    def main_loop(self):
        while True:
            try:
                # Use the input function to get user input                
                line = input("metta> ")
                # If the line isn't empty, evaluate it
                if line:
                    result = runner.run(line)
                    if result is not None:
                        print(result)
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

if __name__ == "__main__":
    repl = REPL()
    readline.add_history("!(AddBeliefEvent (((ExtSet garfield) --> cat) (1.0 0.9)))")
    readline.add_history("!(AddBeliefEvent (((cat x sky) --> like) (1.0 0.9)))")
    readline.add_history("!(AddBeliefEvent ((sky --> (IntSet blue)) (1.0 0.9)))")
    readline.add_history("!(EternalQuestion (((ExtSet garfield) x (IntSet blue)) --> like))")
    for arg in sys.argv:
        if arg.startswith("suggestion1="):
            readline.add_history(arg.split("suggestion1=")[1])
        if arg.startswith("suggestion2="):
            readline.add_history(arg.split("suggestion2=")[1])
    filename = "./../RUN.metta"
    for arg in sys.argv:
        if arg.startswith("file="):
            filename = arg.split("file=")[1]
    with open(filename) as f:
        runner.run(f.read())
    repl.main_loop()

