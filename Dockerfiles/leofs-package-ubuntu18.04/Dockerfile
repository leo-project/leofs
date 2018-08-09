FROM ubuntu:18.04

RUN set -xe \
    && apt-get update \
    && apt-get -y install build-essential libtool libncurses5-dev libssl-dev cmake check \
                          curl git fakeroot libsystemd-dev \
                          patch g++ lsb-release debhelper liblzo2-dev liblzo2-dev findutils \
                          netcat-openbsd sysstat wget sudo

RUN set -xe \
    && curl -O https://raw.githubusercontent.com/kerl/kerl/master/kerl \
    && chmod a+x kerl \
    && mkdir -p ~/bin \
    && mv kerl ~/bin \
    && echo "export PATH=$PATH:~/bin" >> ~/.bashrc \
    && echo 'KERL_CONFIGURE_OPTIONS="--disable-hipe --enable-smp-support --enable-threads --enable-kernel-poll --enable-systemd" ' > ~/.kerlrc \
    && ~/bin/kerl build 20.3 20.3_systemd \
    && mkdir -p ~/erlang/20.3_systemd \
    && ~/bin/kerl install 20.3_systemd ~/erlang/20.3_systemd/ \
    && echo "source ~/erlang/20.3_systemd/activate" >> ~/.bashrc \
    && git clone https://github.com/leo-project/leofs_package.git \
    && mkdir ~/deb \
    && cp leofs_package/deb/make_deb.sh ~/deb \
    && cp leofs_package/common/check_version.sh ~/deb

RUN set -xe \
    && apt-get -y install systemd \
    && wget https://github.com/github/hub/releases/download/v2.5.0/hub-linux-amd64-2.5.0.tgz \
    && tar xzf hub-linux-amd64-2.5.0.tgz \
    && ./hub-linux-amd64-2.5.0/install

ADD run_packaging.sh /root/deb

WORKDIR /root/deb

CMD bash
