FROM python:3.7-bullseye

WORKDIR /home

RUN curl -sSL https://get.haskellstack.org/ | sh

# Install sudo, vim and dependencies.
RUN apt-get update \
    && apt-get install sudo \
    && apt-get install -y llvm-11 llvm-11-dev \
    && apt-get install -y libnuma-dev \
    && apt-get install -y vim

# Install solvers
COPY scripts/ scripts/
RUN sh scripts/ci/install-mathsat-linux.sh \
    && sh scripts/ci/install-cvc5-linux.sh \
    && if [ $(arch) = "aarch64" ]; then sh scripts/ci/install-z3-from-source.sh; else sh scripts/ci/install-z3-linux-amd64.sh; fi \
    && rm -rf scripts/

# Install horus-checker
COPY stack.yaml horus-checker/
COPY horus-check.cabal horus-checker/
COPY app/ horus-checker/app/
COPY src/ horus-checker/src/
RUN cd horus-checker/ \
    && stack --install-ghc setup
RUN cd horus-checker/ \
    && stack install --local-bin-path /usr/local/bin/ \
    && cd ../ \
    && rm -rf horus-checker/

# Install horus-compile
COPY horus-compile/ horus-compile/
RUN pip3 install horus-compile/ \
    && rm -rf horus-compile/
