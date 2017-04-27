# Interface: REST-API
## Configuration

Update LeoGateway's protocol configuration to `rest` in your [LeoGateway configuration](https://github.com/leo-project/leofs/blob/1.3.2.1/apps/leo_gateway/priv/leo_gateway.conf)


```ini
## --------------------------------------------------------------------
## GATEWAY Protocol
## --------------------------------------------------------------------
## Gateway Protocol to use: [s3 | rest | embed | nfs]
protocol = rest
```

## Interface
### Description of LeoFS’ behavior for each HTTP verb

| HTTP Verb | LeoFS’ Behavior |
|-----------|-----------------|
| PUT/POST  | Insert an object into the storage cluster |
| GET       | Retrieve an object from the storage cluster |
| DELETE    | Remove an object from the storage cluster |

### URL format

* URL format: http[s]://`<HOST>`:8080/`<FILEPATH>`
	* LeoFS only uses the `<FILEPATH>` which part of the URL to identify objects.
	* You can check that an object exists in a LeoFS' cluster by using `leofs-adm whereis` command.

```bash
$ leofs-adm whereis <FILEPATH>
```


## Examples
### POST/PUT

```bash
$ curl -X POST -H "Content-Type:image/jpg" \
          --data-binary @test_1.jpg https://hostname:8080/_test/_image/file.png

$ curl -X PUT -H "Content-Type:image/jpg" \
          --data-binary @test_2.jpg https://hostname:8080/_test/_image/file.png
```

### GET

```bash
$ curl -X GET https://hostname:8080/_test/_image/test_2.jpg
```

### DELETE

```bash
$ curl -X DELETE https://hostname:8080/_test/_image/file.png
```


## Related Links

* [For Administrators / Settings / LeoGateway Settings](/admin/settings/leo_gateway.md)