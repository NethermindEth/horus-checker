# FROM python:3.7-bullseye
FROM haskell:latest

WORKDIR /home

# RUN curl -sSL https://get.haskellstack.org/ | sh

# # Install horus-compile
# COPY horus-compile/ horus-compile/
# RUN pip3 install poetry \
#     && pip3 install horus-compile/
# RUN rm -rf horus-compile/

COPY ./ horus-checker/

# Install sudo and compatible version of llvm
RUN apt-get update \
    && apt-get install sudo \
    && apt-get install -y llvm-11 llvm-11-dev

# Install solvers
RUN sh horus-checker/scripts/ci/install-mathsat-linux.sh \
    && sh horus-checker/scripts/ci/install-cvc5-linux.sh \
    && sh horus-checker/scripts/ci/install-z3-linux-from-source.sh

# Install horus-checker
RUN cd horus-checker/ \
    && stack --install-ghc setup \
    && stack install --local-bin-path /usr/local/bin/

# RUN rm -rf horus-checker/
