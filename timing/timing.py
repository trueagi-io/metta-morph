import os
import time

#Tests to run
tests = [
  "!(factorial 30)", 
  "!(range 1 30)", 
  "!(TupleCount (1 2 3 4 5 6 7 8 9 10 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30))",
"""!(StampDisjoint (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30) 
                   (1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30))"""
]
#Run and time then:
workdir = os.getcwd()
for test in tests:
    os.chdir(workdir)
    os.system("cp timing.metta RUN.metta")
    with open("RUN.metta", "a") as file:
        file.write(test)
    os.chdir("../")
    os.system("sh run.sh ./timing/RUN.metta cat-only")
    os.system("sh compile_scheme.sh")
    os.system("./RUN > /dev/null") #make sure binary is already in file cache
    print(f"TEST: {test}")
    t1 = time.time()
    os.system("metta RUN.metta")
    t2 = time.time()
    os.system("./RUN")
    t3 = time.time()
    DIFF1 = (t2-t1)
    DIFF2 = (t3-t2)
    SPEEDUP = DIFF1/DIFF2
    print(f"TIME METTA:\t{DIFF1} seconds")
    print(f"TIME SCHEME:\t{DIFF2} seconds")
    print(f"SPEEDUP:\t{SPEEDUP} times faster")
