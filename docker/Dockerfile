FROM ubuntu:latest
MAINTAINER Jun Takizawa <fire@jun-t.com>
ENV LEOFS_VER=1.2.8 PATH=/usr/local/erlang/17.4/bin:$PATH
WORKDIR /docker
RUN apt-get -y update && \
    apt-get -y upgrade && \
    apt-get -y install openssh-server wget git telnet build-essential libtool libncurses5-dev libssl-dev cmake check && \
    useradd -d /home/leofs -m -s /bin/bash leofs && \
    echo leofs:leofs | chpasswd;echo 'leofs ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers && \
    wget http://www.ivmaisoft.com/_bin/atomic_ops/libatomic_ops-7.4.2.tar.gz && \
    wget http://www.erlang.org/download/otp_src_17.4.tar.gz && \
    tar xvfz libatomic_ops-7.4.2.tar.gz && \
    cd libatomic_ops-7.4.2 && \
    ./configure --prefix=/usr/local && \
    make && \
    make install && \
    cd .. && \
    tar xvfz otp_src_17.4.tar.gz && \
    cd otp_src_17.4 && \
    ./configure \
    --prefix=/usr/local/erlang/17.4 \
    --enable-smp-support \
    --enable-m64-build \
    --enable-halfword-emulator \
    --enable-kernel-poll \
    --without-javac \
    --disable-native-libs \
    --disable-hipe \
    --disable-sctp \
    --enable-threads \
    --with-libatomic_ops=/usr/local && \
    make && \
    make install && \
    cd .. && \
    git clone https://github.com/leo-project/leofs.git && \
    cd leofs && \
    git checkout $LEOFS_VER && \
    make && \
    make release && \
    mkdir -p /usr/local/leofs/bin && \
    cp -r package/* /usr/local/leofs && \
    cp leofs-adm /usr/local/bin && \
    cd .. && \
    rm -rf otp_src_17.4* libatomic_ops-7.4.2* leofs && \
    touch /usr/local/leofs/bin/start_leofs.sh && \
    chmod 755 /usr/local/leofs/bin/start_leofs.sh && \
    echo '#!/usr/bin/env sh' >> /usr/local/leofs/bin/start_leofs.sh && \
    echo '/etc/init.d/ssh start' >> /usr/local/leofs/bin/start_leofs.sh && \
    echo '/usr/local/leofs/leo_manager_0/bin/leo_manager start' >> /usr/local/leofs/bin/start_leofs.sh && \
    echo '/usr/local/leofs/leo_manager_1/bin/leo_manager start' >> /usr/local/leofs/bin/start_leofs.sh && \
    echo '/usr/local/leofs/leo_storage/bin/leo_storage start' >> /usr/local/leofs/bin/start_leofs.sh && \
    echo 'sleep 30' >> /usr/local/leofs/bin/start_leofs.sh && \
    echo 'leofs-adm start' >> /usr/local/leofs/bin/start_leofs.sh && \
    echo '/usr/local/leofs/leo_gateway/bin/leo_gateway start' >> /usr/local/leofs/bin/start_leofs.sh && \
    echo 'echo "LeoFS is ready!!"' >> /usr/local/leofs/bin/start_leofs.sh && \
    echo 'tail -f /dev/null' >> /usr/local/leofs/bin/start_leofs.sh
EXPOSE 22 8080 10010 10011 10020 10021
CMD ["/usr/local/leofs/bin/start_leofs.sh"]
