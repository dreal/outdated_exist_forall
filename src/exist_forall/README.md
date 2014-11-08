∃∀-dReal
========


Requirements
--------------

 - [dReal][dreal]
 - [OCaml][ocaml] (≥ 4.01.0)
 - [batteries][ocaml-batteries] (≥ 2.2.0)
 - [oasis][oasis] (≥ 0.4.4)
 - [ocamlfind][ocamlfind] (≥ 1.5.1)

[dreal]: http://dreal.cs.cmu.edu
[ocaml]: https://ocaml.org
[ocaml-batteries]: http://batteries.forge.ocamlcore.org
[oasis]: http://oasis.forge.ocamlcore.org
[ocamlfind]: http://projects.camlcity.org/projects/findlib.html


Install Requirements on Ubuntu
------------------------------

```bash
sudo apt-get install ocaml opam
opam init
opam switch 4.02.1
opam install batteries oasis ocamlfind


Install Requirements on OSX
---------------------------

```bash
brew install ocaml opam
opam init
opam install batteries oasis ocamlfind
```


How to Compile
--------------

```bash
git clone git@github.com:dreal/exist_forall.git
cd exist_forall/src
make
```


How to Use
----------

```bash
./ef_main.native <input_file>
```
