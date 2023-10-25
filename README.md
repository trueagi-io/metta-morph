# metta-morph
Metta-morph (from Metamorphosis): Macro-based MeTTa to (Chicken) Scheme translator.

The goal is to have a sufficient and fast (hundreds of times faster) subset of MeTTa implemented as an elegant Scheme library, rather than full language capability.

The thin Scheme compatibility layer is achieved via hygienic macros which utilize matching and ambivalence operations.

**Installation**

1. Install MeTTa interpreter: https://github.com/trueagi-io/hyperon-experimental

2. Install chicken Scheme: https://www.call-cc.org/

3. Install the following chicken dependencies:
```
chicken-install srfi-69
chicken-install matchable
chicken-install amb
```

**Run code file with interpreter**

```sh run.sh filename.metta```

which will produce RUN.scm and RUN.metta from filename.metta and run
them with the MeTTa interpreter and Chicken Scheme interpreter.
The output will be timed, and compared with each other, showing either ```==``` or ```!=```.
If ```!=``` is shown then the recommended practice is to adjust the file by adding list annotations etc. until it will show with ```==```.
Hence ```run.sh``` is a workable tool to make sure your code is independent from the particular interpreter implementation.

**Compile code file to binary**

```sh build.sh```

which will build ```RUN.scm``` into a binary ```RUN``` one can execute with ```./RUN``` or ```time ./RUN```.

**Run tests**

```sh test.sh```

The file name of tests which led to different outputs with MeTTa and Chicken Scheme get printed out.

**Current limitations**

- Atoms need to start with ```'```, e.g. ```'fish``` rather than ```fish```. Else Chicken Scheme would treat it is a variable, and MeTTa is neutral to whether a ```'``` is in the beginning.

- Variables need to start with ```$``` as MeTTa demands it, and for Chicken Scheme it does not matter.

- When a variable from the outer scope is again used in ```case``` and ```let``` statements, in MeTTa it will be constrained to have the same value as in the outer scope while in Chicken Scheme the outer variable will be ```shadowed``` within the local context. Just do not use same variable names again and both will behave the same. If equality constraint is necessary, just use an ```If``` statement.

- Basic type annotations are mostly MeTTa-compatible and used to generate more performant code, but the type system is not fully compatible.

- Partial evaluation, e.g. leaving variables as variables when calling a function is not supported.

These limitations are relatively minor, since a major useful part of MeTTa is already supported and the toolset allows to ensure compatibility is incrementally preserved during development. Also, compared to the MeTTa interpreter the code executes by a factor of 5-10x faster with the Chicken Scheme interpreter, and usually 200x+ faster with the compiler.


