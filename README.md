# metta-morph
Metta-morph (from Metamorphosis): Macro-based MeTTa to (Chicken) Scheme translator.

The goal is to have a sufficient and fast (hundreds of times faster) subset of MeTTa implemented as an elegant Scheme library, rather than full language capability.

The thin Scheme compatibility layer is achieved via hygienic macros which utilize matching and ambivalence operations.

**Installation**

1. Install MeTTa interpreter: https://github.com/trueagi-io/hyperon-experimental

2. Install Chicken Scheme: https://www.call-cc.org/

3. Install the following chicken dependencies (install_dependencies.sh):
```
chicken-install srfi-69
chicken-install matchable
chicken-install amb
```

**Run part of your code compiled from metta interpreter**

In folder "extend":

compileme.metta:
```
(= (facF $n)
   (If (== $n 0)
       1
       (* $n (facF (- $n 1)))))
```

yourfile.metta:
```
!(extend-py! mettamorph)
!(compile! compileme.metta)
!(facF 42)
```

All functions defined in compile.me will automatically be compiled and callable,
whereby on next run it will not be compiled again until changes to compileme.metta are made.

Another option is to compile code in-line:

yourfile.metta:
```
!(extend-py! mettamorph)
!(compile! "
(= (facF $n)
   (If (== $n 0)
       1
       (* $n (facF (- $n 1)))))
")
!(facF 42)
```

In both cases simply run with

```
metta yourfile.metta
```

**Run code file with interpreter**

```sh run.sh filename.metta```

which will produce RUN.scm and RUN.metta from filename.metta and run
them with the MeTTa interpreter and Chicken Scheme interpreter.
The output will be timed, and compared with each other, showing either ```==``` or ```!=```.
If ```!=``` is shown then the recommended practice is to adjust the file appropriately with limitations in mind, until it will show with ```==```.
Hence ```run.sh``` is a workable tool to make sure your code is independent from the particular interpreter implementation.

**Compile code file to binary**

```sh compilescheme.sh```

which will build ```RUN.scm``` into a binary ```RUN``` one can execute with ```./RUN``` or ```time ./RUN```.

**Run tests**

```sh test.sh```

The file name of tests which led to different outputs with MeTTa and Chicken Scheme get printed out.

**Current limitations**

- When a variable from the outer scope is again used in ```case``` and ```let``` statements, in MeTTa it will be constrained to have the same value as in the outer scope while in Chicken Scheme the outer variable will be ```shadowed``` within the local context. Do not introduce same variable names again in inner scope and both will behave the same. If equality constraint is intended, just use an explicit ```If``` statement.

- Basic type annotations are mostly MeTTa-compatible and used to generate more performant code, but the type system is not fully compatible. With Mettamorph as extension library the MeTTa type system can be fully used.

- Partial evaluation, e.g. leaving variables as variables when calling a function is not supported.

These limitations are relatively minor, a substantial part of MeTTa is supported. Most importantly, the toolset allows to ensure compatibility with the MeTTa interpreter is incrementally preserved during development and the compiled code can easily be called with the MeTTa interpreter. Compared to the MeTTa interpreter the code executes by a factor of 10x+ faster with the Chicken Scheme interpreter, and usually 200x+ faster with the compiler.

**Complex example**

The most complex example to date is running full metta-nars (not just minnars.metta and nalifier.metta which is part of the test suite) with Mettamorph.
It uses the metta-morph branch of metta-nars which only contains minor code modifications to run metta-nars 100 times faster:
https://github.com/patham9/metta-nars/tree/metta-morph
Run ```sh install_nars.sh``` to obtain it, and the included scripts show how code concatenation and compilation to a binary with an injected REPL can be controlled.
