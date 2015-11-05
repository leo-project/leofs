# LeoFS's ls-command benchmark tool

#### leo-ls-bench's usage
```bash
usage: leofs-ls-bench [--help]
       leofs-ls-bench -c <concurrency> -d <target-dir> -k <kind_of_client>
                      [-num-of-sub-dirs <number-of-sub-dirs>] [--debug]

       description of the parameters:
         * <kind_of_client>: [s3cmd | nfs]
```


### Run leofs-ls-bench
#### TEST
```bash
$ leofs-ls-bench -c <concurrency> -d <target-dir> -k <s3cmd> --debug
```
#### Run
* Without sub-directories

```bash
$ leofs-ls-bench -c <concurrency> -d <target-dir> -k <kind_of_client>

## example:
$ leofs-ls-bench -c 3 -d test/ -k s3cmd
```

* Within sub-directories

```bash
$ leofs-ls-bench -c <concurrency> -d <target-dir> -k <kind_of_client> -num-of-sub-dirs <number-of-sub-dirs>

## example:
##   - <num-of-sub-dirs> affects the processing.
##     In this case, "test/sub_1", "test/sub_2" and "test/sub_3" will be requested.
$ leofs-ls-bench -c 3 -d test/sub_ -k s3cmd -num-of-sub-dirs 3

```

#### Log file
After executing a benchmark, you're able to find the log-file as below:

* ``leofs-ls-bnech-result.<unixtime>.log``


## License
[Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0)

## Sponsors

LeoProject/LeoFS is sponsored by [Rakuten, Inc.](http://global.rakuten.com/corp/) and supported by [Rakuten Institute of Technology](http://rit.rakuten.co.jp/).
