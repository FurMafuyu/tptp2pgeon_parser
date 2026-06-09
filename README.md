# TPTP to PGeon parser

Currently translates problem files from TPTP and QMLTP to PGeon and Tableaux Workbench problem files.

### Supported
- PGeon
- Tableaux Workbench

### Requierements
- Dune
- Menhir
- OCamllex

### Usage

dune exec parser -- --<options> <problem_file_path> [> <output_file>]

### Options
- --pgeon
- --twb