#!/bin/sh

if [ $# -ne 1 ]; then
  echo "Usage: ./git_checkout.sh <leo_libs_docs dir>"
  exit 1
fi

for lib in leo_backend_db \
           leo_cache \
           leo_commons \
           leo_dcerl \
           leo_logger \
           leo_mcerl \
           leo_mq \
           leo_object_storage \
           leo_ordning_reda \
           leo_pod \
           leo_redundant_manager \
           leo_rpc \
           leo_s3_libs \
           leo_statistics
do
    echo $lib
    cd "deps/$lib" && sh ./make_rst_doc.sh && cp doc/rst/* $1/$lib/
    cd -
done
echo "Finished"
