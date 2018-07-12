FROM leoproject/leofs-build-deps:0.1

WORKDIR /root

ENV GOPATH="$HOME/go"

RUN set -xe \
    && apt-get update \
    # deps
    && apt-get -y install libcurl4-openssl-dev openjdk-7-jdk ant \
                          php5 php5-curl php-aws-sdk \
                          ruby ruby-dev libmagic-dev python-pip \
    ## for aws-sdk-ruby
    && gem install aws-sdk \
    && gem install rack -v=1.6.4 \
    && gem install jeweler -v=1.6.4 \
    && gem install content_type --conservative \
    ## for boto/boto3
    && pip install boto boto3 filechunkio \
    ## for aws-sdk-go
    && mkdir $GOPATH \
    && curl -o go.tgz https://dl.google.com/go/go1.6.4.linux-amd64.tar.gz \
    && echo "b58bf5cede40b21812dfa031258db18fc39746cc0972bc26dae0393acc377aaf *go.tgz" | sha256sum -c - \
    && tar -C /usr/local -xzf go.tgz \
    && rm go.tgz \
    && export PATH="/usr/local/go/bin:$PATH" \
    && go get github.com/aws/aws-sdk-go/service/s3 \
    # build leofs_client_tests
    && git clone https://github.com/leo-project/leofs_client_tests.git \
    && cd leofs_client_tests \
    && git submodule update -i \
    ## generate test data
    && cd temp_data \
    && ./gen.sh \
    && cd .. \
    ## prep for aws-sdk-cpp
    && cd aws-sdk-cpp \
    && mkdir build \
    && cd build \
    && cmake -DBUILD_ONLY="s3" .. \
    && make \
    && cd ../.. \
    ## prep for aws-sdk-php
    && cd aws-sdk-php \
    && curl -sS https://getcomposer.org/installer | php \
    && php composer.phar install

ENV PATH $GOPATH/bin:/usr/local/go/bin:$PATH

ADD run_test.sh /root/leofs_client_tests/

CMD /root/leofs_client_tests/run_test.sh
