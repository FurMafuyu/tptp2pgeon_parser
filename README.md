# TPTPTPTP : TPTP Translation Parser for Tableaux and PGeon

Translates problem files from TPTP and QMLTP to PGeon and Tableau Workbench problem files.

### Supported
- PGeon (--pgeon)
- Tableaux Workbench (--twb)
- Any modal logic (--modal "name") that is specified in the problem file's matrix

### Requierements
- Dune
- Menhir
- OCamllex

### Usage
dune exec parser -- --<options> --modal <logic_name> <problem_file_path> [> <output_file>]

dune exec parser -- --pgeon --modal s5 ../../Problems/Originals/GLC181+1.p > GLC181+1.txt