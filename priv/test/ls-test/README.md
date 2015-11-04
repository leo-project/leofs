# LeoFS's ls-command benchmark tool

#### leo-ls-bench's usage
```bash
usage: leofs-ls-bench [--help]
       leofs-ls-bench -c <concurrency> -d <target-dir> -k <kind_of_client>
                      [--debug]

       description of the parameters:
         * <kind_of_client>: [s3cmd | nfs]
```


### Run leofs-ls-bench
#### TEST
```bash
$ leofs-ls-bench -c 10 -d <target-dir> -k <s3cmd> --debug
```
#### Run
```bash
$ leofs-ls-bench -c 10 -d <target-dir> -k <nfs>
```

#### Log file
After executing a benchmark, you're able to find the log-file as below:

* ``leofs-ls-bnech-result.<unixtime>.log``


## License
[Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0)

## Sponsors

LeoProject/LeoFS is sponsored by [Rakuten, Inc.](http://global.rakuten.com/corp/) and supported by [Rakuten Institute of Technology](http://rit.rakuten.co.jp/).