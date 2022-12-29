FROM python:3.7-bullseye

WORKDIR /home

RUN curl -sSL https://get.haskellstack.org/ | sh

# Install horus-compile
COPY horus-compile/ horus-compile/
RUN pip3 install poetry \
    && pip3 install horus-compile/
RUN rm -rf horus-compile/

# Install horus-checker
COPY ./ horus-checker/
RUN apt-get update \
    && apt-get install sudo \
    && sh horus-checker/scripts/ci/install-mathsat-linux.sh \
    && sh horus-checker/scripts/ci/install-z3-linux.sh \
    && sh horus-checker/scripts/ci/install-cvc5-linux.sh \
    && apt-get install -y llvm-13 llvm-13-dev
RUN cd horus-checker/ \
    && stack install --local-bin-path /usr/local/bin/

RUN rm -rf horus-checker/
