# To build this image, Dockerfile needs to be place in
# the same directory as horus-checker/ and horus-compile/ directories.
# i.e.: ls command should output:
# Dockerfile horus-checker/ horus-compile/

FROM python:3.7-bullseye

WORKDIR /home

RUN curl -sSL https://get.haskellstack.org/ | sh

# Install horus-checker
COPY ./ horus-checker/
RUN apt-get update \
    && apt-get install sudo \
    && sh horus-checker/scripts/ci/install-mathsat-linux.sh \
    && sh horus-checker/scripts/ci/install-z3-linux.sh
RUN cd horus-checker/ \
    && stack install --local-bin-path /usr/local/bin/

RUN rm -rf horus-checker/

# Install horus-compile
COPY horus-compile/ horus-compile/
RUN pip3 install poetry \
    && pip3 install horus-compile/
RUN rm -rf horus-compile/
