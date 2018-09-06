# second-order-checker
## Compilation
OCaml and OCamlbuild are needed to compile the project, then in the root directory:
```sh
make
```
## Usage
Once compiled, the program can be used with the command line:
```sh
./main.native game_fil.gm output_dir
```
### Example with untyped lambda calculus
The game file lamb.gm is provided, a Makefile and a parser are already written in the lamb directory.
```sh
./main.native lamb.gm lamb/src
```
The three files ast.ml, unify.ml and checker.ml are now generated in lamb/src.
```sh
cd lamb && make
```
lamb/main.native is generated and can check derivation trees given on stdin.
```sh
./main.native < test
```
The output is empty, the derivation tree is accepted.
