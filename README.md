# Faial: finds bugs in CUDA kernels

# Binary distribution

### [Download `faial` for Linux x86-64bits](https://gitlab.com/umb-svl/faial/-/jobs/artifacts/main/raw/bundle/faial.tar.bz2?job=bundle&inline=false)

<!--
### [Download `faial` for Windows x86-64bits](https://ci.appveyor.com/api/projects/cogumbreiro/faial/artifacts/faial-win64.zip)
-->

Instructions:
1. Extract the binary distribution of [`faial.tar.bz2`](https://gitlab.com/umb-svl/faial/-/jobs/artifacts/master/raw/bundle/faial.tar.bz2?job=bundle) and ensure that `bin` is in your `PATH`
2. Install [`libz3.so >= 4.11.2`](https://github.com/Z3Prover/z3/releases/download/z3-4.11.2/z3-4.11.2-x64-glibc-2.31.zip); Package `libz3-4` in Debian/Ubuntu; `z3-libs` in Fedora.

```
$ tar xvf faial.tar.bz2 
LICENSE
README.md
bin/
bin/faial-drf
bin/c-to-json
bin/cu-to-json
bin/c-ast
bin/faial-bc
bin/faial-bc-dyn
share/
share/c-to-json/
share/c-to-json/include/
...
```


# [Usage and tutorial](tutorial/README.md)

Ensure that your CUDA file has all of its includes available and then run:

```bash
$ faial-drf example.cu
```

Next, feel free to access the [`tutorial/`](tutorial/) directory!

# Build from source

## Dependencies

* [opam `>= 2.0`](https://opam.ocaml.org/)
* [ocamlc `>= 4.14.0`](https://ocaml.org/)

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

# Building from scratch (Ubuntu 22.04)

These instructions are also available as a dockerfile: [`docker/dev.Dockerfile`](docker/dev.Dockerfile)

Install the following system packages:
```
$ sudo apt-get install \
        llvm-dev \
        libclang-dev \
        lld \
        zlib1g-dev \
        ninja-build \
        cmake \
        upx-ucl \
        python2 \
        git \
        wget \
        unzip \
        build-essential \
        libssl-dev \
        pkg-config \
        python3-distutils \
        libgmp-dev
```

Install `c-to-json`:
```
$ git clone https://gitlab.com/umb-svl/c-to-json
$ cd c-to-json
$ make
$ sudo make install # installs to /usr/local/bin
$ cd ..
```

Install `ocaml 4.14.0`:
```
$ # Install OCaml's package manager:
$ sudo wget https://github.com/ocaml/opam/releases/download/2.1.4/opam-2.1.4-x86_64-linux -O /usr/bin/opam
$ sudo chmod a+x /usr/bin/opam
$ # Install OCaml 4.14
$ opam init --compiler=4.14.0
$ # Set environment variables
$ eval $(opam env)
```

Install `faial`:
```
$ cd faial
$ ./configure.sh
$ make
$ sudo cp /home/faial/faial/faial-drf /usr/local/bin/
$ sudo cp /home/faial/faial/faial-bc /usr/local/bin/
$ sudo cp /home/faial/faial/c-ast /usr/local/bin/
```
