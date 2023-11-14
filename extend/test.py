import os
import ast
import sys
basefile = ""
filename = sys.argv[1]
if filename.split("/")[-1][0].isupper() or "compileme.metta" in filename: #temp files are skipped
    exit(0)
with open(filename) as file:
    basefile = file.read()
print("Testing:", sys.argv[1])
newfile = ""
for line in basefile.split("\n"):
    if not line.startswith('")') and not line.startswith("))") and \
       not line.startswith("!(compile! ") and not line.startswith("!(extend-py mettamorph)"):
           newfile += line + "\n"
    if "!(compile! " in line and ".metta)" in line:
        includefile = "./extend/" + line.split("!(compile! ")[1].split(")")[0]
        with open(includefile) as file:
            newfile += file.read() + "\n"
        
with open("TEST.metta","w") as file:
    file.write(newfile)

os.system(f"metta {filename} > OUTPUT_IS.txt")
os.system(f"metta TEST.metta > OUTPUT_SHOULD.txt")

with open("OUTPUT_IS.txt") as file:
    OUTPUT_IS = file.read().replace("[(Compilation: skipped)]\n", "").replace("[(Compilation: success)]\n", "").replace("#t", "True").replace("#f", "False")
with open("OUTPUT_SHOULD.txt") as file:
    OUTPUT_SHOULD = file.read().replace('"','').replace("'","")

def SORT_LINES(name):
    lines = ""
    for line in name.split("\n"):
        try:
            my_list = ast.literal_eval(list_as_string)
            my_list.sort()
            lines += str(my_list) + "\n"
        except:
            lines += line + "\n"

OUTPUT_IS = SORT_LINES(OUTPUT_IS)
OUTPUT_SHOULD = SORT_LINES(OUTPUT_SHOULD)
if OUTPUT_IS != OUTPUT_SHOULD:
    print("FAILED:", filename)

