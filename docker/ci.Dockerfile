FROM registry.gitlab.com/umb-svl/faial/faial:base

#############################################
# CI/CD specific

USER faial

# Install Ocaml dependencies
ADD configure.sh /
RUN eval $(opam config env) && \
    sh /configure.sh -y

