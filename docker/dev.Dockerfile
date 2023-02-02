FROM ubuntu:22.04

ARG DEBIAN_FRONTEND="noninteractive"
ENV TZ="America/New_York"

USER root

RUN apt-get update && \
    apt-get install --yes \
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
        libgmp-dev \
        && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

RUN useradd \
      --user-group \
      --shell /bin/bash \
      --create-home \
      faial

USER faial
WORKDIR /home/faial

############################################
# c-to-json
ARG C_TO_JSON_VERSION=master
RUN git clone https://gitlab.com/umb-svl/c-to-json && \
    cd c-to-json && \
    git checkout ${C_TO_JSON_VERSION} && \
    make
USER root
RUN cd /home/faial/c-to-json && \
    make install


############################################
# OCaml and dependencies
ARG FAIAL_VERSION=main
RUN \
  wget https://github.com/ocaml/opam/releases/download/2.1.4/opam-2.1.4-x86_64-linux -O /usr/bin/opam && \
  chmod a+x /usr/bin/opam
USER faial
RUN \
  opam init --compiler=4.14.0 --disable-sandboxing --yes && \
  wget https://gitlab.com/umb-svl/faial/-/raw/${FAIAL_VERSION}/configure.sh && \
  eval $(opam env) && \
  bash configure.sh -y && \
  rm -f configure.sh

############################################
# Faial
RUN \
    eval $(opam env) && \
    git clone https://gitlab.com/umb-svl/faial && \
    cd faial && \
    ./configure.sh -y && \
    git checkout ${FAIAL_VERSION} && \
    make

USER root
RUN \
  cp /home/faial/faial/faial-drf /usr/local/bin/ && \
  cp /home/faial/faial/faial-bc /usr/local/bin/ && \
  cp /home/faial/faial/c-ast /usr/local/bin/

USER faial
RUN faial-drf faial/tutorial/saxpy.cu
