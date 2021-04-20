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

ARG C_TO_JSON_VERSION=master

RUN git clone https://gitlab.com/umb-svl/c-to-json && \
    cd c-to-json && \
    git checkout ${C_TO_JSON_VERSION} && \
    make

USER root
RUN cd /home/faial/c-to-json && \
    make install

ARG FAIL_INFER_VERSION=master
USER faial
RUN git clone https://gitlab.com/umb-svl/faial-infer && \
    cd faial-infer && \
    git checkout ${FAIAL_INFER_VERSION} && \
    USER=faial pyoxidizer build --release

USER root
RUN \
    cp /home/faial/faial-infer/build/x86_64-unknown-linux-gnu/release/install/faial-infer /usr/local/bin && \
    chmod a+x /usr/local/bin/faial-infer

USER faial

ARG FAIAL_VERSION=master
RUN \
     git clone https://gitlab.com/umb-svl/faial && \
    cd faial && \
    ./configure.sh -y && \
    git checkout ${FAIAL_INFER_VERSION} && \
    eval $(opam env)  && \
    make && \
    make ui

USER root
RUN \
  cp /home/faial/faial/faial-bin /usr/local/bin && \
  cp /home/faial/faial/faial-ui/target/release/faial /usr/local/bin

USER faial
RUN faial faial/tutorial/saxpy.cu