FROM ubuntu:20.04

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
        z3 && \
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

#############################################
# CI/CD specific


# Install Ocaml dependencies
ADD configure.sh /
RUN eval $(opam config env) && \
    sh /configure.sh -y

# Install Rust dependencies
ADD --chown=faial faial-ui/Cargo.toml faial-ui/Cargo.lock /opt/faial-ui/
RUN cd /opt/faial-ui && \
    mkdir src && \
    echo "fn main() {}" > src/main.rs && \
    cargo b --release && \
    cargo b && \
    rm -f src/main.rs target/{debug,release}/faial{,.d} Cargo.{toml,lock} && \
    rmdir src

USER root

# Required by `run-tests.py`
RUN apt-get update && \
    apt-get install --yes python3 && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

USER faial