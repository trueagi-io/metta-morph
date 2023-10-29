import os
import json
import ctypes
from ctypes import *
from hyperon.ext import register_atoms
from hyperon import *

class PatternOperation(OperationObject):
    def __init__(self, name, op, unwrap=False, rec=False):
        super().__init__(name, op, unwrap)
        self.rec = rec
    def execute(self, *args, res_typ=AtomType.UNDEFINED):
        return super().execute(*args, res_typ=res_typ)

def wrapnpop(func):
    def wrapper(*args):
        a = [str("'"+arg) if arg is SymbolAtom else str(arg) for arg in args]
        res = func(*a)
        return [res]
    return wrapper

def func_mettamorph(evalstr):
    bytes_literal = evalstr.encode('utf-8')
    input_str = c_char_p(bytes_literal)
    result = mettamorphlib.mettamorph(input_str)
    result_str = ctypes.string_at(result).decode('utf-8')
    return result_str

def call_mettamorph(*a):
    tokenizer = Tokenizer()
    EXPRESSION = "(" + (" ".join([str(x).replace("(quote ", "[").replace("(", "(list ").replace("[", "(quote ") for x in a])) + ")"
    parser = SExprParser(str(func_mettamorph(EXPRESSION)))
    return parser.parse(tokenizer)

def call_compilefile(*a):
    global mettamorphlib
    loadfile = a[0][1:-1]
    if not loadfile.endswith(".metta"):
        content = loadfile
        loadfile = "TEMP.metta"
        AlreadyWritten = False
        if os.path.exists(loadfile):
            with open(loadfile, "r") as file:
                if content == file.read():
                    AlreadyWritten = True
        if not AlreadyWritten:
            with open(loadfile, "w") as file:
                file.write(content)
    TEMPfiles = loadfile.replace(".metta", "").upper()
    lastmodification = os.path.getmtime(loadfile)
    status, fcompiles = ("success", "COMPILATIONS.json")
    if os.path.exists(fcompiles):
        with open(fcompiles) as file:
            compilations = json.loads(file.read())
        if loadfile in compilations and compilations[loadfile] == lastmodification:
            status = "skipped"
    else:
        compilations = dict([])
    if status != "skipped":
        cwd = os.getcwd()
        os.chdir("./../")
        os.system(f"sh runscheme.sh ./extend/{loadfile} cat-only")
        os.chdir(cwd)
        os.system(f"cat ./../RUN.scm cinterface.scm > {TEMPfiles}.scm")
        os.system(f"csc {TEMPfiles}.scm cinterface.c -shared")
        compilations[loadfile] = lastmodification
        with open(fcompiles, 'w') as file:
             file.write(json.dumps(compilations))
    # Load the DLL
    mettamorphlib = ctypes.CDLL(f"{TEMPfiles}.so")
    result = mettamorphlib.CHICKEN_INIT()
    # Define the argument and return types for the mettamorph function
    mettamorphlib.mettamorph.argtypes = [ctypes.c_char_p]
    mettamorphlib.mettamorph.restype = ctypes.c_char_p
    return E(S("Compile:"), S(status))

@register_atoms
def scheme_atoms():
    call_mettamorph_atom = G(PatternOperation('mettamorph', wrapnpop(call_mettamorph), unwrap=False))
    call_compilefile_atom = G(PatternOperation('compile!', wrapnpop(call_compilefile), unwrap=False))
    return { r"compile!": call_compilefile_atom, r"mettamorph": call_mettamorph_atom }
