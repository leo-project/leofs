FROM leoproject/leofs-all-in-one:latest

WORKDIR /leofs

RUN set -xe \
    # install NFS utility and deps for leo_nfs_integration_tests.sh
    && apt-get -y install nfs-common uuid-runtime \
    ## modify leo_gateway.conf to use NFS
    && sed -e "s/s3/nfs/g" apps/leo_gateway/priv/leo_gateway.conf > apps/leo_gateway/priv/leo_gateway.conf.new \
    && mv apps/leo_gateway/priv/leo_gateway.conf.new apps/leo_gateway/priv/leo_gateway.conf \
    ## modify leo_storage.conf to change the AVS dir
    && sed -e "s/\.\/avs/\/ssd\/avs/g" apps/leo_storage/priv/leo_storage.conf > apps/leo_storage/priv/leo_storage.conf.new \
    && mv apps/leo_storage/priv/leo_storage.conf.new apps/leo_storage/priv/leo_storage.conf \
    ## initialize the AVS dir for the NFS test
    && mkdir -p /mnt/leofs \
    && mkdir /ssd \
    && (cd apps/leo_gateway/test/; tar xzf nfs_dummy_test_avs.tar.gz -C /ssd) \
    && USER=root make release_for_test

Add run_nfs_test.sh /leofs

CMD bash
