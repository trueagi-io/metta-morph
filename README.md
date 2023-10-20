# metta-morph
Metta-morph (from Metamorphosis): Macro-based MeTTa to (Chicken) Scheme translator.

Please note: the goal is to have a sufficient and fast (hundred of times faster) subset of MeTTa implemented as an elegant Scheme library, rather than full language capability.

**Installation**

1. Install MeTTa interpreter: https://github.com/trueagi-io/hyperon-experimental

2. Install chicken Scheme: https://www.call-cc.org/

3. Install chicken dependencies:
```
chicken-install amb
chicken-install srfi-69
chicken-install matchable
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

- Function calls need to be distinguished from data, e.g. sometimes instead of (1 2 3) one needs to write (list 1 2 3). While the list token will be omitted in the translation to MeTTa, it is crucial to be present for Chicken Scheme in many cases in order to give the same output.

- Atoms need to start with ```'```, e.g. ```'fish``` rather than ```fish```. Else Chicken Scheme would think it is a variable, and MeTTa is neutral to whether a ```'``` is in the beginning.

- Variables need to start with ```$``` as MeTTa demands it, and for Chicken Scheme it does not matter.

- When a variable from the outer scope is again used in ```case``` and ```let``` statements, in MeTTa it will be constrained to have the same value as in the outer scope while in Chicken Scheme the outer variable will be ```shadowed``` within the local context. Just do not use same variable names again and both will behave the same. If equality constraint is necessary, just use an ```If``` statement.

- The MeTTa type system is not part of the Mettamorph language subset, but at least primitive (builtin) type annotations will soon also be supported in metta-morph. For now, simply avoid type annotations altogether.

These limitations are all minor, the biggest incompatibilities have already been resolved with hygienic macros which utilize matching and ambivalence operations, and the speed benefits are more than just "significant".


