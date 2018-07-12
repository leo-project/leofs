FROM leoproject/leofs-build-deps:0.1

ARG REPO_BRANCH="develop"

RUN set -xe \
    # install leofs
    && git clone https://github.com/leo-project/leofs.git \
    && cd leofs \
    && git checkout $REPO_BRANCH \
    ## add -q option for netcat(nc) to close the client socket and nc works as expected
    && sed -e "s/-N/-q 1/g" leofs-adm > leofs-adm.new \
    && mv leofs-adm.new leofs-adm \
    && chmod +x leofs-adm \
    && make \
    && cd .. \ 
    # install leofs_test2
    && git clone https://github.com/leo-project/leofs_test2.git \
    && cd leofs_test2 \
    && make

## install s3cmd for delete-bucket tests
RUN set -xe \
    && apt-get -y install s3cmd \
    && cd /leofs \
    && sed -e "s/\.\/boot/USER=root \.\/boot/g" test/test_it_del_bucket.sh > test/test_it_del_bucket.sh.new \
    && cp test/test_it_del_bucket.sh.new test/test_it_del_bucket.sh

Add .s3cfg /root
Add run_leofs_test2.sh /leofs

WORKDIR /leofs

EXPOSE 22 8080 10010 10011 10020 10021

CMD USER=root ./bootstrap.sh start integration-test ; tail -f /dev/null
