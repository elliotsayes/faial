FROM registry.gitlab.com/umb-svl/faial/faial:base

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

USER faial
