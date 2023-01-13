# FROM python:3.7-bullseye
FROM haskell:buster

WORKDIR /home

# RUN curl -sSL https://get.haskellstack.org/ | sh

# Install sudo and compatible version of llvm
RUN apt-get update \
    && apt-get install sudo \
    && apt-get install -y llvm-11 llvm-11-dev

# Install solvers
COPY scripts/ scripts/
RUN sh scripts/ci/install-mathsat-linux.sh \
    && sh scripts/ci/install-cvc5-linux.sh \
    && sh scripts/ci/install-z3-linux-from-source.sh \
    && rm -rf scripts/

# Install horus-checker
COPY stack.yaml horus-checker/
COPY horus-check.cabal horus-checker/
COPY app/ horus-checker/app/
COPY src/ horus-checker/src/
RUN cd horus-checker/ \
    && stack --install-ghc setup \
    && stack install --local-bin-path /usr/local/bin/ \
    && cd ../ \
    && rm -rf horus-checker/

# Install horus-compile
COPY horus-compile/ horus-compile/
RUN curl -sSL https://bootstrap.pypa.io/get-pip.py | python3 \
    && apt-get update && apt-get install -y python3-dev \
    && pip3 install horus-compile/ \
    && rm -rf horus-compile/
