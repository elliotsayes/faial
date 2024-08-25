# Faial: finds bugs in CUDA kernels

# Binary distribution

### [Download `faial` for Linux x86-64bits](https://gitlab.com/umb-svl/faial/-/jobs/artifacts/main/raw/bundle/faial.tar.bz2?job=bundle-lin&inline=false)
### [Download `faial` for macOS arm64 (M1/M2/M3)](https://gitlab.com/umb-svl/faial/-/jobs/artifacts/main/raw/bundle/faial.tar.bz2?job=bundle-mac&inline=false)

Instructions:
1. Create a directory to hold the binary distribution, say `/opt/faial`
2. Extract the binary distribution archive `faial.tar.bz2`
3. (Optional) Add `/opt/faial` to the `PATH`, or run directly `/opt/faial/faial-drf`

# [Usage and tutorial](tutorial/README.md)

Ensure that your CUDA file has all of its includes available and then run:

```bash
$ faial-drf example.cu
```

Next, feel free to access the [`tutorial/`](tutorial/) directory!

# Build from source

## Dependencies

* [opam `>= 2.0`](https://opam.ocaml.org/)
* [ocamlc `>= 5.1`](https://ocaml.org/)

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

Install `ocaml 5.1.1`:
```
$ # Install OCaml's package manager:
$ sudo wget https://github.com/ocaml/opam/releases/download/2.1.4/opam-2.1.4-x86_64-linux -O /usr/bin/opam
$ sudo chmod a+x /usr/bin/opam
$ # Install OCaml 5.1.1
$ opam init --compiler=5.1.1
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
