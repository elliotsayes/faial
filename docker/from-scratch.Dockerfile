FROM ubuntu:20.04

ARG DEBIAN_FRONTEND="noninteractive"
ENV TZ="America/New_York"

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
        z3 \
        llvm-dev \
        libclang-dev \
        build-essential \
        m4 \
        git \
        lld \
        ninja-build \
        cmake \
        upx-ucl \
        python \
        python3 \
        libssl-dev \
        pkg-config \
        && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN useradd -m faial
USER faial

WORKDIR /home/faial

ARG OCAML_VERSION=4.11.1

# Install OCaml
RUN \
    opam init --bare --disable-sandboxing && \
    opam switch create main ${OCAML_VERSION} && \
    eval $(opam env) && \
    opam install -y depext && \
    opam depext -y conf-m4.1

# Install Rust
ARG RUST_VERSION=1.47.0
ENV PATH="$PATH:/home/faial/.cargo/bin"
RUN wget https://sh.rustup.rs -O - | \
    sh -s -- -y --default-toolchain ${RUST_VERSION} --profile minimal

# faial-infer pre-reqs
RUN cargo install pyoxidizer --version 0.8.0
USER faial