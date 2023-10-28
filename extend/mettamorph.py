import os
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
    result = mylib.mettamorph(input_str)
    result_str = ctypes.string_at(result).decode('utf-8')
    return result_str

def call_mettamorph(*a):
    tokenizer = Tokenizer()
    EXPRESSION = "(" + (" ".join([str(x) for x in a])) + ")"
    parser = SExprParser(str(func_mettamorph(EXPRESSION)))
    return parser.parse(tokenizer)

@register_atoms
def scheme_atoms():
    call_mettamorph_atom = G(PatternOperation('mettamorph', wrapnpop(call_mettamorph), unwrap=False))
    return { r"mettamorph": call_mettamorph_atom }

if "run-only" not in sys.argv:
	cwd = os.getcwd()
	os.chdir("./../")
	os.system("sh runscheme.sh ./extend/accelerated.metta cat-only")
	os.chdir(cwd)
	os.system("cat ./../RUN.scm cinterface.scm > ACCELERATED.scm")
	os.system("csc ACCELERATED.scm cinterface.c -shared")

# Load the DLL
mylib = ctypes.CDLL('ACCELERATED.so')
result = mylib.CHICKEN_INIT()

# Define the argument and return types for the mettamorph function
mylib.mettamorph.argtypes = [ctypes.c_char_p]
mylib.mettamorph.restype = ctypes.c_char_p
