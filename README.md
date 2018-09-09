# second-order-checker
## Compilation
OCaml and OCamlbuild are needed to compile the project, then in the root directory:
A makefile is provided which compiles everything and generates all the checkers of the checkers directory.
```sh
make
```
The game maker and all the checkers are compiled.
## Usage
You can compile the game maker alone.
```sh
cd gmc && make
```
Once compiled, the game marker (gmc directory) can be used with the command line:
```sh
./main.native game_fil.gm output_dir
```
It generates 3 files (ast.ml, checker.ml and unify.ml) in the output directory.

### Example with untyped lambda calculus
The game file lamb.gm is provided with the parser in the checkers/lamb directory.

In order to generate a checker, you first need to compile the game maker, then:
```sh
cd gmc
./main.native ../checkers/lamb.gm ../checkers/lamb/src
```
The three files ast.ml, unify.ml and checker.ml are now generated in lamb/src.
```sh
ln -sf game_src ../checkers/lamb/default
make -C checkers/lamb -f ../../gmc/Makefile.game
```
The folder of common files is now linked into the directory, the Makefile can now find them.
lamb/main.native is generated and can check derivation trees given on stdin.
```sh
cd checkers/lamb
./main.native < test
```
The output is empty, the derivation tree is accepted.
## How to write a new checker
Two files need to be provided: a game file and a partial parser.

#TODO
