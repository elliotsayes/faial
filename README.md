# GPU Contracts

[![Build status](https://ci.appveyor.com/api/projects/status/n2uv6o1mpl18w5ir?svg=true)](https://ci.appveyor.com/project/cogumbreiro/faial)

A modeling language to specify and debug GPU programs.

### [Download `faial` for Linux x86-64bits](https://gitlab.com/umb-svl/faial/-/jobs/artifacts/master/download?job=build-dist)

### [Download `faial` for Windows x86-64bits](https://ci.appveyor.com/api/projects/cogumbreiro/faial/artifacts/faial-win64.zip)

# Compiling from source

### 1. Setup

**Run this once.** The following command will install all dependencies needed to build the project.

```bash
./configure.sh
```

### 2. Build

**Run this to build the binary.**

```bash
$ make
```


## External dependencies

* [z3](https://github.com/Z3Prover/z3)
* [opam](https://opam.ocaml.org/)


# Usage

```
*** DATA RACE ERROR ***
+------+--------+
|      | global |
+------+--------+
| SIZE |   0    |
+------+--------+
+---------+-----+-----+
|         | T1  | T2  |
+---------+-----+-----+
| $access |  w  |  w  |
+---------+-----+-----+
| $index  | [0] | [0] |
+---------+-----+-----+
```

For more information run `./faial --help`
