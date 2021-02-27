FROM ocaml/opam:ubuntu-20.10-ocaml-4.11
RUN sudo apt-get update && sudo apt-get install --yes \
        python3 \
        python3-dev \
        python3-pip \
        build-essential \
        libffi8ubuntu1 \
        m4 \
        git \
        libffi-dev \
        libgmp-dev \
        z3 \
    && \
    opam repo add opam https://opam.ocaml.org && \
    opam depext -y conf-m4.1
ENV PATH="$PATH:/home/opam/.cargo/bin"
RUN curl https://sh.rustup.rs -sSf | \
    sh -s -- -y --default-toolchain 1.47.0 --profile minimal

COPY configure.sh requirements.txt /

RUN eval $(opam config env) && \
    /configure.sh && \
    sudo pip3 install -r /requirements.txt

ADD --chown=opam faial-ui/Cargo.toml faial-ui/Cargo.lock /opt/faial-ui/
RUN cd /opt/faial-ui && \
    mkdir src && \
    echo "fn main() {}" > src/main.rs && \
    cargo b --release && \
    cargo b && \
    rm -f src/main.rs target/{debug,release}/faial{,.d} Cargo.{toml,lock} && \
    rmdir src
