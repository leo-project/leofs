FROM erlang:19.3.6.9-slim

RUN set -xe \
    && apt-get update \
    && apt-get -y install build-essential libtool libncurses5-dev libssl-dev cmake check curl git g++ lsb-release \
    && apt-get -y install sudo netcat-traditional

CMD tail -f /dev/null
