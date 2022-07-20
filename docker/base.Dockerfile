FROM ubuntu:22.04

# python3 is required by `run-tests.py` and z3
# python3-distutils is required by z3

RUN apt-get update && \
    apt-get install --yes \
        opam \
        build-essential \
        m4 \
        git \
        wget \
        tree \
        libffi-dev \
        libgmp-dev \
        python3 \
        python3-distutils \
        && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

ENV PATH="$PATH:/opt/z3/bin"
ARG Z3_VERSION=4.8.8
ARG Z3_ARCH=x64-ubuntu-16.04
RUN cd /opt && \
    wget https://github.com/Z3Prover/z3/releases/download/z3-${Z3_VERSION}/z3-${Z3_VERSION}-${Z3_ARCH}.zip && \
    unzip z3-${Z3_VERSION}-${Z3_ARCH}.zip && \
    mv z3-${Z3_VERSION}-${Z3_ARCH} z3 && \
    rm -f z3-${Z3_VERSION}-${Z3_ARCH}.zip && \
    z3 --version

RUN useradd -m faial
USER faial

WORKDIR /home/faial

ARG OCAML_VERSION=4.14.0

# Install OCaml
RUN \
    opam init --bare --disable-sandboxing && \
    opam switch create main ${OCAML_VERSION} && \
    eval $(opam env) 

# Install Rust
ARG RUST_VERSION=1.59.0
ENV PATH="$PATH:/home/faial/.cargo/bin"
RUN wget https://sh.rustup.rs -O - | \
    sh -s -- -y --default-toolchain ${RUST_VERSION} --profile minimal

