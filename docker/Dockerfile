FROM ocaml/opam2:alpine-3.10-ocaml-4.07
RUN sudo apk --no-cache add --update \
        python3 \
        python3-dev \
        py-pip build-base \
        libffi \
        m4 \
        libffi-dev \
        gmp-dev \
        z3 \
    && \
    opam repo add opam https://opam.ocaml.org && \
    opam depext -y conf-m4.1
ADD configure.sh requirements.txt /
RUN eval $(opam config env) && \
    /configure.sh && \
    sudo pip3 install -r /requirements.txt
