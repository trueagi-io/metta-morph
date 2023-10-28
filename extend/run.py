from mettamorph import *

metta = MeTTa()

with open("notaccelerated.metta", 'r') as file:
    mettacode = file.read()

print(metta.run(mettacode))
