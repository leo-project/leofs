FROM ubuntu:18.04

RUN set -xe \
    && apt-get update \
    && apt-get -y install git wget \
    && wget https://github.com/github/hub/releases/download/v2.5.0/hub-linux-amd64-2.5.0.tgz \
    && tar xzf hub-linux-amd64-2.5.0.tgz \
    && ./hub-linux-amd64-2.5.0/install \
    && git clone https://github.com/leo-project/leofs.git

WORKDIR /leofs

CMD bash
