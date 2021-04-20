# Faial - DRF checking CUDA kernels

[![Build status](https://ci.appveyor.com/api/projects/status/n2uv6o1mpl18w5ir?svg=true)](https://ci.appveyor.com/project/cogumbreiro/faial)

# Install

### [Download `faial` for Linux x86-64bits](https://gitlab.com/umb-svl/faial/-/jobs/artifacts/master/raw/bundle/faial.tar.bz2?job=bundle)

<!--
### [Download `faial` for Windows x86-64bits](https://ci.appveyor.com/api/projects/cogumbreiro/faial/artifacts/faial-win64.zip)
-->

Instructions:
1. Extract the binary distribution of [`faial.tar.bz2`](https://gitlab.com/umb-svl/c-to-json) and ensure that `bin` is in your `PATH`
2. Install [`z3`](https://github.com/Z3Prover/z3/releases)


```
$ tar xvf faial.tar.bz2 
LICENSE
README.md
bin/
bin/cu-to-json
bin/faial-infer
bin/c-to-json
bin/faial
bin/faial-bin
share/
share/c-to-json/
share/c-to-json/include/
share/c-to-json/include/stdarg.h
share/c-to-json/include/cuda_textures.h
share/c-to-json/include/cuda_curand.h
share/c-to-json/include/cuda.h
share/c-to-json/include/stddef.h
```


# [Usage and tutorial](tutorial/README.md)

Ensure that your CUDA file has all of its includes available and then run:

```bash
$ faial example.cu
```

Feel free to access the  [tutorial/](tutorial/)

# Build from source

## Run-time dependencies

* [`z3`](https://github.com/Z3Prover/z3/releases)
* [`c-to-json`](https://gitlab.com/umb-svl/c-to-json)
* [`faial-infer`](https://gitlab.com/umb-svl/faial-infer/)

## Compile-time dependencies

* Rust `>= 1.4.7`
* OCaml `>= 4.11.0`
* opam `>= 2.0`


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

---

# Building from scratch
