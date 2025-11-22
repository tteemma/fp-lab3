# Lab 3 – Streaming interpolation in OCaml

This project implements a command-line tool `lab3` that performs streaming
interpolation over a sequence of `(x, y)` points read from standard input.

Features:

- Piecewise-linear interpolation (`--linear`)
- Newton interpolation on a sliding window of N points (`--newton N`)
- Configurable step size for output grid (`--step h`)
- Streaming behaviour: output is produced as new points are read

## Usage

```sh
dune build
_build/default/src/main.exe --linear --step 0.7
```

Input format: one point per line, for example

```text
0 0
1 1
2 4
```

Separators can be spaces, tabs, commas or semicolons.

The program writes interpolated points to standard output in the form

```text
linear: 0 0
newton: 0.5 0.5
```
