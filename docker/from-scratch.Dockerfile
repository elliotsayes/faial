FROM registry.gitlab.com/umb-svl/faial/faial:base

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
        libssl-dev \
        pkg-config \
        && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

ARG C_TO_JSON_VERSION=master

RUN git clone https://gitlab.com/umb-svl/c-to-json && \
    cd c-to-json && \
    git checkout ${C_TO_JSON_VERSION} && \
    make

USER root
RUN cd /home/faial/c-to-json && \
    make install

USER faial

ARG FAIAL_VERSION=master
RUN \
     git clone https://gitlab.com/umb-svl/faial && \
    cd faial && \
    ./configure.sh -y && \
    git checkout ${FAIAL_VERSION} && \
    eval $(opam env)  && \
    make && \
    make ui

USER root
RUN \
  cp /home/faial/faial/faial-bin /usr/local/bin && \
  cp /home/faial/faial/faial-ui/target/release/faial /usr/local/bin

USER faial
RUN faial faial/tutorial/saxpy.cu
