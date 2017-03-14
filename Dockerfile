FROM debian:8

# must update repos before installing
RUN apt-get update

# install ocam and other deps
# z3 Deps: python2
# javalib/sawja Deps: zlib1g-dev
# ocamlfind Deps: m4
# project Deps: openjdk-7-jdk
# scheme-format Deps: chicken-bin libchicken-dev
RUN apt-get install -y \
    m4 \
    zlib1g-dev \
    python \
    openjdk-7-jdk \
    chicken-bin \
    libchicken-dev \
    opam \
    oasis \
    ocaml

# set up scheme-format helper binary
RUN git clone 'https://github.com/russellw/scheme-format.git' && \
    cd scheme-format && \
    git checkout f03ebe75fd4e7a5f22f40744d37d0f67c20d3bd8 && \
    csc main.scm -o scheme-format && \
    mv scheme-format /usr/bin/ && \
    cd .. && \
    rm -rf scheme-format

# setup opam and install ocam libraries
RUN opam init -y && eval $(opam config env) && opam install -y \
    core \
    sawja

# build z3 with ml bindings
# stay on version 4.5.0 for now
COPY z3-patches /z3-patches
RUN git clone 'https://github.com/Z3Prover/z3.git' && \
    cd z3 && \
    git checkout tags/z3-4.5.0 && \
    eval $(opam config env) && \
    ls /z3-patches/*.patch && git apply --whitespace=nowarn /z3-patches/*.patch && \
    python scripts/mk_make.py --ml && \
    cd build && \
    make && \
    make install && \
    cd ../.. && \
    rm -rf z3

# Add config to .bashrc
RUN echo 'eval $(opam config env)' >> /root/.bashrc

