%%======================================================================
%%
%% Leo S3 HTTP
%%
%% Copyright (c) 2012-2015 Rakuten, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%%======================================================================

%% HTTP METHODS
-define(HTTP_GET,    <<"GET">>).
-define(HTTP_POST,   <<"POST">>).
-define(HTTP_PUT,    <<"PUT">>).
-define(HTTP_DELETE, <<"DELETE">>).
-define(HTTP_HEAD,   <<"HEAD">>).

%% HTTP-RELATED
-define(SERVER_HEADER, {<<"server">>,<<"LeoFS">>}).
-define(STR_NEWLINE,   "\n").
-define(STR_SLASH,     "/").
-define(BIN_SLASH,     <<"/">>).
-define(BIN_EMPTY,     <<>>).

-undef(DEF_SEPARATOR).
-define(DEF_SEPARATOR, <<"\n">>).

-define(DEF_DELIMITER, <<"/">>).
-define(ERR_TYPE_INTERNAL_ERROR, internal_server_error).

%% HTTP HEADER
-define(HTTP_HEAD_AGE,                <<"age">>).
-define(HTTP_HEAD_AUTHORIZATION,      <<"authorization">>).
-define(HTTP_HEAD_CACHE_CTRL,         <<"cache-control">>).
-define(HTTP_HEAD_CONTENT_LENGTH,     <<"content-length">>).
-define(HTTP_HEAD_CONTENT_MD5,        <<"content-md5">>).
-define(HTTP_HEAD_CONTENT_TYPE,       <<"content-type">>).
-define(HTTP_HEAD_DATE,               <<"date">>).
-define(HTTP_HEAD_ETAG,               <<"etag">>).
-define(HTTP_HEAD_IF_MODIFIED_SINCE,  <<"if-modified-since">>).
-define(HTTP_HEAD_LAST_MODIFIED,      <<"last-modified">>).
-define(HTTP_HEAD_PREFIX,             <<"prefix">>).
-define(HTTP_HEAD_RANGE,              <<"range">>).
-define(HTTP_HEAD_CONTENT_ENCODING,   <<"content-encoding">>).

-define(HTTP_HEAD_RESP_AGE,               <<"Age">>).
-define(HTTP_HEAD_RESP_CACHE_CTRL,        <<"Cache-Control">>).
-define(HTTP_HEAD_RESP_CONTENT_LENGTH,    <<"Content-Length">>).
-define(HTTP_HEAD_RESP_CONTENT_MD5,       <<"Content-MD5">>).
-define(HTTP_HEAD_RESP_CONTENT_TYPE,      <<"Content-Type">>).
-define(HTTP_HEAD_RESP_CONTENT_RANGE,     <<"Content-Range">>).
-define(HTTP_HEAD_RESP_ETAG,              <<"ETag">>).
-define(HTTP_HEAD_RESP_IF_MODIFIED_SINCE, <<"If-Modified-Since">>).
-define(HTTP_HEAD_RESP_LAST_MODIFIED,     <<"Last-Modified">>).

-define(HTTP_HEAD_X_AMZ_META_DIRECTIVE,         <<"x-amz-metadata-directive">>).
-define(HTTP_HEAD_X_AMZ_COPY_SOURCE,            <<"x-amz-copy-source">>).
-define(HTTP_HEAD_X_AMZ_ID_2,                   <<"x-amz-id-2">>).
-define(HTTP_HEAD_X_AMZ_REQ_ID,                 <<"x-amz-request-id">>).
-define(HTTP_HEAD_X_AMZ_ACL,                    <<"x-amz-acl">>).
-define(HTTP_HEAD_X_AMZ_CONTENT_SHA256,         <<"x-amz-content-sha256">>).
-define(HTTP_HEAD_X_AMZ_DECODED_CONTENT_LENGTH, <<"x-amz-decoded-content-length">>).
-define(HTTP_HRAD_X_AMZ_DATE,                   <<"x-amz-date">>).
-define(HTTP_HEAD_X_AMZ_META_DIRECTIVE_COPY,    <<"COPY">>).
-define(HTTP_HEAD_X_AMZ_META_DIRECTIVE_REPLACE, <<"REPLACE">>).
-define(HTTP_HEAD_X_AMZ_LEOFS_FROM_CACHE,       <<"x-amz-meta-leofs-from-cache">>).
-define(HTTP_HEAD_X_FROM_CACHE,                 <<"x-from-cache">>).


%% @see: http://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-header-based-auth.html
-define(HTTP_HEAD_X_VAL_AWS4_SHA256, <<"STREAMING-AWS4-HMAC-SHA256-PAYLOAD">>).
-define(HTTP_HEAD_X_AWS_SIGNATURE_V2, <<"AWS ">>).
-define(HTTP_HEAD_X_AWS_SIGNATURE_V4, <<"AWS4">>).

-define(AWS_SIGNATURE_V4_SHA256_KEY,  <<"AWS4-HMAC-SHA256-PAYLOAD">>).
-define(AWS_SIGNATURE_V4_SHA256_HASH, <<"e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855">>).
%% leo_hex:binary_to_hex(crypto:hash(sha256, <<"">>))


-define(HTTP_CTYPE_OCTET_STREAM, <<"application/octet-stream">>).
-define(HTTP_CTYPE_XML,          <<"application/xml">>).

-define(HTTP_QS_BIN_ACL,         <<"acl">>).
-define(HTTP_QS_BIN_UPLOADS,     <<"uploads">>).
-define(HTTP_QS_BIN_UPLOAD_ID,   <<"uploadId">>).
-define(HTTP_QS_BIN_PART_NUMBER, <<"partNumber">>).
-define(HTTP_QS_BIN_MARKER,      <<"marker">>).
-define(HTTP_QS_BIN_MAXKEYS,     <<"max-keys">>).
-define(HTTP_QS_BIN_MULTI_DELETE,<<"delete">>).
-define(HTTP_QS_BIN_DELIMITER,   <<"delimiter">>).
-define(HTTP_QS_BIN_VERSIONING,  <<"versioning">>).
-define(HTTP_QS_BIN_LOCATION,    <<"location">>).

-define(HTTP_ST_OK,                  200).
-define(HTTP_ST_NO_CONTENT,          204).
-define(HTTP_ST_PARTIAL_CONTENT,     206).
-define(HTTP_ST_NOT_MODIFIED,        304).
-define(HTTP_ST_BAD_REQ,             400).
-define(HTTP_ST_FORBIDDEN,           403).
-define(HTTP_ST_NOT_FOUND,           404).
-define(HTTP_ST_CONFLICT,            409).
-define(HTTP_ST_BAD_RANGE,           416).
-define(HTTP_ST_INTERNAL_ERROR,      500).
-define(HTTP_ST_NOT_IMPLEMENTED,     501).
-define(HTTP_ST_SERVICE_UNAVAILABLE, 503).
-define(HTTP_ST_GATEWAY_TIMEOUT,     504).

-define(HTTP_MAXKEYS_LIMIT,          1000).
-define(HTTP_METADATA_LIMIT,         2048).

-define(CACHE_HTTP,  'http').
-define(CACHE_INNER, 'inner').
-type(cache_method() :: ?CACHE_HTTP | ?CACHE_INNER).

%% Default values
-define(DEF_HTTP_PORT,                8080).
-define(DEF_HTTP_SSL_PORT,            8443).
-define(DEF_HTTP_SSL_C_FILE,          "./server_cert.pem").
-define(DEF_HTTP_SSL_K_FILE,          "./server_key.pem").
-define(DEF_HTTP_NUM_OF_ACCEPTORS,    32).
-define(DEF_HTTP_CUSTOM_HEADER_CONF,  "./http_custom_header.conf").
-define(DEF_HTTP_TIMEOUT_FOR_HEADER,  5000).
-define(DEF_HTTP_TIMEOUT_FOR_BODY,    15000).
-define(DEF_HTTP_SEND_CHUNK_LEN,      5242880).
-define(DEF_HTTP_CACHE,               false).
-define(DEF_HTTP_MAX_KEEPALIVE,       1024).
-define(DEF_CACHE_WORKERS,            64).
-define(DEF_CACHE_RAM_CAPACITY,       64000000).
-define(DEF_CACHE_DISC_CAPACITY,      64000000).
-define(DEF_CACHE_DISC_THRESHOLD_LEN, 1000000).
-define(DEF_CACHE_DISC_DIR_DATA,      "./cache/data").
-define(DEF_CACHE_DISC_DIR_JOURNAL,   "./cache/journal").
-define(DEF_CACHE_EXPIRE,             300).
-define(DEF_CACHE_MAX_CONTENT_LEN,    1000000).
-define(DEF_LOBJ_MAX_CHUNKED_OBJS,       1000).
-define(DEF_LOBJ_MAX_LEN_OF_OBJ,         524288000).
-define(DEF_LOBJ_CHUNK_OBJ_LEN,          5242880).
-define(DEF_LOBJ_READING_CHUNK_OBJ_LEN,  5242880). %% since v0.16.8
-define(DEF_LOBJ_THRESHOLD_OF_CHUNK_LEN, 5767168).
-define(DEF_S3API_MAX_KEYS, 1000).
-define(DEF_MAX_NUM_OF_METADATAS, 50).
-define(DEF_DONT_ABORT_CLEANUP, false).

%% error codes used in a error response
-define(XML_ERROR_CODE_EntityTooLarge, "EntityTooLarge").
-define(XML_ERROR_CODE_InvalidArgument, "InvalidArgument").
-define(XML_ERROR_CODE_InvalidRequest, "InvalidRequest").
-define(XML_ERROR_CODE_AccessDenied, "AccessDenied").
-define(XML_ERROR_CODE_NoSuchKey, "NoSuchKey").
-define(XML_ERROR_CODE_InvalidRange, "InvalidRange").
-define(XML_ERROR_CODE_InternalError, "InternalError").
-define(XML_ERROR_CODE_ServiceUnavailable, "ServiceUnavailable").
-define(XML_ERROR_CODE_SlowDown, "SlowDown").
-define(XML_ERROR_CODE_BucketAlreadyExists, "BucketAlreadyExists").
-define(XML_ERROR_CODE_BucketAlreadyOwnedByYou, "BucketAlreadyOwnedByYou").
-define(XML_ERROR_CODE_OperationAborted, "OperationAborted").
-define(XML_ERROR_CODE_MalformedXML, "MalformedXML").
-define(XML_ERROR_CODE_BadDigest, "BadDigest").
-define(XML_ERROR_CODE_InvalidBucketName, "InvalidBucketName").
-define(XML_ERROR_CODE_SignatureDoesNotMatch, "SignatureDoesNotMatch").
-define(XML_ERROR_CODE_RequestTimeTooSkewed, "RequestTimeTooSkewed").
-define(XML_ERROR_CODE_MetadataTooLarge, "MetadataTooLarge").
-define(XML_ERROR_CODE_InvalidPart, "InvalidPart").
-define(XML_ERROR_CODE_NoSuchUpload, "NoSuchUpload").

%% error messages used in a error response
-define(XML_ERROR_MSG_EntityTooLarge, "Your proposed upload exceeds the maximum allowed object size.").
-define(XML_ERROR_MSG_InvalidArgument, "Invalid Argument").
-define(XML_ERROR_MSG_InvalidRequest, "SOAP requests must be made over an HTTPS connection.").
-define(XML_ERROR_MSG_AccessDenied, "Access Denied").
-define(XML_ERROR_MSG_NoSuchKey, "The specified key does not exist.").
-define(XML_ERROR_MSG_InvalidRange, "The requested range cannot be satisfied.").
-define(XML_ERROR_MSG_InternalError, "We encountered an internal error. Please try again.").
-define(XML_ERROR_MSG_ServiceUnavailable, "Please reduce your request rate.").
-define(XML_ERROR_MSG_SlowDown, "Please reduce your request rate.").
-define(XML_ERROR_MSG_BucketAlreadyExists, "Please select a different name and try again.").
-define(XML_ERROR_MSG_BucketAlreadyOwnedByYou, "Your previous request to create the named bucket succeeded and you already own it.").
-define(XML_ERROR_MSG_OperationAborted, "A conflicting conditional operation is currently in progress against this resource. Try again.").
-define(XML_ERROR_MSG_MalformedXML, "The XML you provided was not well-formed or did not alidate against our published schema").
-define(XML_ERROR_MSG_BadDigest, "The Content-MD5 you specified did not match what we received.").
-define(XML_ERROR_MSG_InvalidBucketName, "The specified bucket is not valid.").
-define(XML_ERROR_MSG_SignatureDoesNotMatch, "The request signature we calculated does not match the signature you provided. Check your AWS secret access key and signing method.").
-define(XML_ERROR_MSG_RequestTimeTooSkewed, "The difference between the request time and the server's time is too large.").
-define(XML_ERROR_MSG_MetadataTooLarge, "Your metadata headers exceed the maximum allowed metadata size.").
-define(XML_ERROR_MSG_InvalidPart, "One or more of the specified parts could not be found. The part might not have been uploaded, or the specified entity tag might not have matched the part's entity tag.").
-define(XML_ERROR_MSG_NoSuchUpload, "The specified multipart upload does not exist. The upload ID might be invalid, or the multipart upload might have been aborted or completed.").

%% Macros
%% - code:200
-define(reply_ok(_H,_R),
        cowboy_req:reply(?HTTP_ST_OK,_H,_R)).
%% - code:200 with body
-define(reply_ok(_H,_B,_R),
        cowboy_req:reply(?HTTP_ST_OK,_H,_B,_R)).
%% - code:204
-define(reply_no_content(_H,_R),
        cowboy_req:reply(?HTTP_ST_NO_CONTENT,_H,_R)).
%% - code:206
-define(reply_partial_content(_H,_R),
        cowboy_req:reply(?HTTP_ST_PARTIAL_CONTENT,_H,_R)).
%% - code:206 with body
-define(reply_partial_content(_H,_B,_R),
        cowboy_req:reply(?HTTP_ST_PARTIAL_CONTENT,_H,_B,_R)).
%% - code:304
-define(reply_not_modified(_H,_R),
        cowboy_req:reply(?HTTP_ST_NOT_MODIFIED,_H,_R)).

%% for HEAD(without body)
%% - code:403
-define(reply_bad_request_without_body(_H,_R),
        cowboy_req:reply(?HTTP_ST_BAD_REQ,_H,_R)).
%% - code:404
-define(reply_not_found_without_body(_H,_R),
        cowboy_req:reply(?HTTP_ST_NOT_FOUND, _H,_R)).
%% - code:500
-define(reply_internal_error_without_body(_H,_R),
        cowboy_req:reply(?HTTP_ST_INTERNAL_ERROR, _H,_R)).
%% - code:501
-define(reply_not_implemented_without_body(_H,_R),
        cowboy_req:reply(?HTTP_ST_NOT_IMPLEMENTED, _H,_R)).
%% - code:503
-define(reply_timeout_without_body(_H,_R),
        cowboy_req:reply(?HTTP_ST_SERVICE_UNAVAILABLE,_H,_R)).

%% for GET/PUT/DELETE(with body)
-define(reply_bad_request(_H, _Code, _Msg, _Key, _ReqId, _R),
        cowboy_req:reply(?HTTP_ST_BAD_REQ, _H,
                         io_lib:format(?XML_ERROR, [_Code, _Msg,
                                                    xmerl_lib:export_text(_Key), _ReqId]), _R)).
-define(reply_forbidden(_H, _Code, _Msg, _Key, _ReqId, _R),
        cowboy_req:reply(?HTTP_ST_FORBIDDEN, _H,
                         io_lib:format(?XML_ERROR, [_Code, _Msg,
                                                    xmerl_lib:export_text(_Key), _ReqId]), _R)).
-define(reply_not_found(_H, _Key, _ReqId, _R),
        cowboy_req:reply(?HTTP_ST_NOT_FOUND, _H,
                         io_lib:format(?XML_ERROR, [?XML_ERROR_CODE_NoSuchKey,
                                                    ?XML_ERROR_MSG_NoSuchKey,
                                                    xmerl_lib:export_text(_Key), _ReqId]), _R)).
-define(reply_conflict(_H, _Code, _Msg, _Key, _ReqId, _R),
        cowboy_req:reply(?HTTP_ST_CONFLICT, _H,
                         io_lib:format(?XML_ERROR, [_Code, _Msg,
                                                    xmerl_lib:export_text(_Key), _ReqId]), _R)).
-define(reply_bad_range(_H, _Key, _ReqId, _R),
        cowboy_req:reply(?HTTP_ST_BAD_RANGE, _H,
                         io_lib:format(?XML_ERROR, [?XML_ERROR_CODE_InvalidRange,
                                                    ?XML_ERROR_MSG_InvalidRange,
                                                    xmerl_lib:export_text(_Key), _ReqId]), _R)).
-define(reply_service_unavailable_error(_H, _Key, _ReqId, _R),
        cowboy_req:reply(?HTTP_ST_SERVICE_UNAVAILABLE, _H,
                         io_lib:format(?XML_ERROR, [?XML_ERROR_CODE_ServiceUnavailable,
                                                    ?XML_ERROR_MSG_ServiceUnavailable,
                                                    _Key, _ReqId]), _R)).
-define(reply_internal_error(_H, _Key, _ReqId, _R),
        cowboy_req:reply(?HTTP_ST_INTERNAL_ERROR, _H,
                         io_lib:format(?XML_ERROR, [?XML_ERROR_CODE_InternalError,
                                                    ?XML_ERROR_MSG_InternalError,
                                                    xmerl_lib:export_text(_Key), _ReqId]), _R)).
-define(reply_timeout(_H, _Key, _ReqId, _R),
        cowboy_req:reply(?HTTP_ST_SERVICE_UNAVAILABLE, _H,
                         io_lib:format(?XML_ERROR, [?XML_ERROR_CODE_SlowDown,
                                                    ?XML_ERROR_MSG_SlowDown,
                                                    xmerl_lib:export_text(_Key), _ReqId]), _R)).
-define(reply_malformed_xml(_H, _R),
        cowboy_req:reply(?HTTP_ST_OK, _H,
                         io_lib:format(?XML_ERROR_2, [?XML_ERROR_CODE_MalformedXML,
                                                      ?XML_ERROR_MSG_MalformedXML,
                                                      "", ""]), _R)).
-define(reply_bad_digest(_H, _Key, _ReqId, _R),
        cowboy_req:reply(?HTTP_ST_BAD_REQ, _H,
                         io_lib:format(?XML_ERROR, [?XML_ERROR_CODE_BadDigest,
                                                    ?XML_ERROR_MSG_BadDigest,
                                                    xmerl_lib:export_text(_Key), _ReqId]), _R)).
-define(reply_metadata_too_large(_H, _Key, _ReqId, _R),
        cowboy_req:reply(?HTTP_ST_BAD_REQ, _H,
                         io_lib:format(?XML_ERROR, [?XML_ERROR_CODE_MetadataTooLarge,
                                                    ?XML_ERROR_MSG_MetadataTooLarge,
                                                    xmerl_lib:export_text(_Key), _ReqId]), _R)).
-define(reply_upload_not_found(_H, _Key, _ReqId, _R),
        cowboy_req:reply(?HTTP_ST_NOT_FOUND, _H,
                         io_lib:format(?XML_ERROR, [?XML_ERROR_CODE_NoSuchUpload,
                                                    ?XML_ERROR_MSG_NoSuchUpload,
                                                    xmerl_lib:export_text(_Key), _ReqId]), _R)).

-define(http_header(_R, _K),
        case cowboy_req:header(_K, _R) of
            {undefined, _} ->
                ?BIN_EMPTY;
            {Bin, _} ->
                Bin
        end).
-define(http_etag(_E),
        lists:append(["\"", leo_hex:integer_to_hex(_E,32), "\""])).
-define(http_date(_D),
        leo_http:rfc1123_date(_D)).
-define(httP_cache_ctl(_C),
        lists:append(["max-age=",integer_to_list(_C)])).
-define(http_content_type(_H),
        case lists:keyfind(?HTTP_HEAD_CONTENT_TYPE,1,_H) of
            false     ->
                ?HTTP_CTYPE_OCTET_STREAM;
            {_, Val} ->
                Val
        end).

%% Special URLs
-define(HTTP_SPECIAL_URL_HEALTH_CHECK, <<"_leofs_adm/ping">>).

%% canned ACLs
-define(acl_read, "READ").
-define(acl_read_acp, "READ_ACP").
-define(acl_write, "WRITE").
-define(acl_write_acp, "WRITE_ACP").
-define(acl_full_control, "FULL_CONTROL").

%% S3 Response XML
-define(XML_BUCKET_LIST,
        lists:append(["<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                      "<ListAllMyBucketsResult xmlns=\"http://s3.amazonaws.com/doc/2006-03-01\">",
                      "<Owner>",
                      "<ID>LeoFS</ID>",
                      "<DisplayName>webfile</DisplayName>",
                      "</Owner>",
                      "<Buckets>",
                      "~s",
                      "</Buckets></ListAllMyBucketsResult>"])).

-define(XML_OBJ_LIST,
        lists:append(["<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                      "<ListBucketResult xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">",
                      "<Name>~s</Name>",
                      "<Prefix>~s</Prefix>",
                      "<Marker></Marker>",
                      "<MaxKeys>~s</MaxKeys>",
                      "<Delimiter>/</Delimiter>",
                      "~s",
                      "<IsTruncated>~s</IsTruncated>",
                      "<NextMarker>~s</NextMarker>",
                      "</ListBucketResult>"])).

-define(XML_OBJ_LIST_HEAD,
        lists:append(["<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                      "<ListBucketResult xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">",
                      "<Name>~s</Name>",
                      "<Prefix>~s</Prefix>",
                      "<Marker></Marker>",
                      "<MaxKeys>~s</MaxKeys>",
                      "<Delimiter>~s</Delimiter>"])).

-define(XML_OBJ_LIST_FILE_1,

        lists:append(["<Contents>",
                      "<Key>~s</Key>",
                      "<LastModified>~s</LastModified>",
                      "<ETag>~s</ETag>",
                      "<Size>~s</Size>",
                      "<StorageClass>STANDARD</StorageClass>",
                      "<Owner>",
                      "<ID>leofs</ID>",
                      "<DisplayName>leofs</DisplayName>",
                      "</Owner>",
                      "</Contents>"])).

-define(XML_OBJ_LIST_FILE_2,
        lists:append(["<Contents>",
                      "<Key>~s~s</Key>",
                      "<LastModified>~s</LastModified>",
                      "<ETag>~s</ETag>",
                      "<Size>~s</Size>",
                      "<StorageClass>STANDARD</StorageClass>",
                      "<Owner>",
                      "<ID>leofs</ID>",
                      "<DisplayName>leofs</DisplayName>",
                      "</Owner>",
                      "</Contents>"])).

-define(XML_OBJ_LIST_FOOT,
        lists:append(["<IsTruncated>~s</IsTruncated>",
                      "<NextMarker>~s</NextMarker>",
                      "</ListBucketResult>"])).

-define(XML_BUCKET,
        lists:append(["<Bucket><Name>~s</Name>",
                      "<CreationDate>~s</CreationDate></Bucket>"])).

-define(XML_BUCKET_LOCATION,
        <<"<VersioningConfiguration xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\"/>">>).

-define(XML_DIR_PREFIX,
        lists:append(["<CommonPrefixes><Prefix>",
                      "~s",
                      "~s",
                      "</Prefix></CommonPrefixes>"])).

-define(XML_COPY_OBJ_RESULT,
        lists:append(["<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                      "<CopyObjectResult>",
                      "<LastModified>~s</LastModified>",
                      "<ETag>\"~s\"</ETag>",
                      "</CopyObjectResult>"])).

-define(XML_UPLOAD_INITIATION,
        lists:append(["<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                      "<InitiateMultipartUploadResult xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">"
                      "<Bucket>~s</Bucket>"
                      "<Key>~s</Key>"
                      "<UploadId>~s</UploadId>"
                      "</InitiateMultipartUploadResult>"])).

-define(XML_UPLOAD_COMPLETION,
        lists:append(["<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                      "<CompleteMultipartUploadResult xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">",
                      "<Location>http://Example-Bucket.s3.amazonaws.com/Example-Object</Location>",
                      "<Bucket>~s</Bucket>",
                      "<Key>~s</Key>",
                      "<ETag>\"~s-~s\"</ETag>",
                      "</CompleteMultipartUploadResult>"])).

-define(XML_ERROR,
        lists:append(["<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                      "<Error>",
                      "<Code>~s</Code>",
                      "<Message>~s</Message>",
                      "<Resource>~s</Resource>",
                      "<RequestId>~s</RequestId>",
                      "</Error>"])).

-define(XML_ERROR_2,
        lists:append(["<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                      "<Error>",
                      "<Code>~s</Code>",
                      "<Message>~s</Message>",
                      "<RequestId>~s</RequestId>",
                      "<HostId>~s</HostId>",
                      "</Error>"])).

-define(XML_ACL_POLICY,
        lists:append(["<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                      "<AccessControlPolicy xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">",
                      "<Owner>",
                      "<ID>~s</ID>",
                      "<DisplayName>~s</DisplayName>",
                      "</Owner>",
                      "<AccessControlList>~s</AccessControlList>",
                      "</AccessControlPolicy>"])).

-define(XML_ACL_GRANT,
        lists:append(["<Grant>",
                      "<Grantee xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:type=\"Group\">",
                      "<URI>~s</URI>",
                      "</Grantee>",
                      "<Permission>~s</Permission>",
                      "</Grant>"])).

-define(XML_MULTIPLE_DELETE,
        lists:append(["<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
                      "<DeleteResult xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\">",
                      "~s",
                      "~s",
                      "</DeleteResult>"])).

-define(XML_MULTIPLE_DELETE_SUCCESS_ELEM,
        lists:append(["<Deleted><Key>",
                      "~s",
                      "</Key></Deleted>"])).

-define(XML_MULTIPLE_DELETE_ERROR_ELEM,
        lists:append(["<Error><Key>",
                      "~s",
                      "</Key><Code>AccessDenied</Code><Message>Access Denied</Message></Error>"])).

-define(XML_BUCKET_VERSIONING,
        <<"<VersioningConfiguration xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\"/>">>).

%% ETS Identifier
-define(ETS_HTTP_OPTION_TBL, 'leo_gateway_http_opts_tbl').
-define(ETS_HTTP_OPTION_KEY, 'leo_gateway_http_opts_key').

%% Records
-type aws_chunk_state() ::  wait_size | wait_head | read_chunk | error | done.

-record(aws_chunk_sign_params, {sign_head       :: binary(),
                                sign_key        :: binary(),
                                prev_sign       :: binary(),
                                chunk_sign      :: binary(),
                                chunk_size      :: non_neg_integer(),
                                hash_context    :: undefined | {crypto:digest_type(), binary()}
                               }).

-record(aws_chunk_decode_state, {buffer         :: binary(),
                                 dec_state      :: aws_chunk_state(),
                                 chunk_offset   :: non_neg_integer(),
                                 sign_params    :: #aws_chunk_sign_params{},
                                 total_len      :: non_neg_integer()
                                }).

-record(http_options, {
          %% for basic
          handler                      :: atom(),         %% http-handler
          port = 0                     :: pos_integer(),  %% http port number
          ssl_port = 0                 :: pos_integer(),  %% ssl port number
          ssl_certfile = []            :: string(),       %% ssl cert file name
          ssl_keyfile = []             :: string(),       %% ssk key file name
          num_of_acceptors = 0         :: pos_integer(),  %% # of acceptors (http server's workers)
          max_keepalive = 0            :: pos_integer(),  %% # of request per processes
          headers_config_file = []     :: string(),       %% HTTP custom header configuration file path
          timeout_for_header           :: pos_integer(),  %% Timeout for reading header
          timeout_for_body             :: pos_integer(),  %% Timeout for reading body
          sending_chunked_obj_len = 0  :: pos_integer(),  %% sending chunk length
          %% for cache
          cache_method                 :: cache_method(), %% cahce method: [http | inner]
          cache_workers = 0            :: pos_integer(),  %% number of chache-fun's workers
          cache_ram_capacity = 0       :: pos_integer(),  %% cache size (RAM)
          cache_disc_capacity = 0      :: pos_integer(),  %% cache size (Disc)
          cache_disc_threshold_len = 0 :: pos_integer(),  %% cache disc threshold length (Disc)
          cache_disc_dir_data = []     :: string(),       %% cache-directory for data    (Disc)
          cache_disc_dir_journal = []  :: string(),       %% cache-directory for journal (Disc)
          cache_expire = 0             :: pos_integer(),  %% cache expire time (sec)
          cache_max_content_len = 0    :: pos_integer(),  %% cache max content length (byte)
          cachable_content_type = []   :: list(),         %% cachable content types
          cachable_path_pattern = []   :: list(),         %% cachable path patterns
          has_disk_cache = false       :: boolean(),      %% Has Disk Cache?
          %% for multipart upload
          dont_abort_cleanup = false   :: boolean(),      %% whether removing related objects when handling abort MU request
          %% for large-object
          max_chunked_objs = 0         :: pos_integer(),  %% max chunked objects
          max_len_of_obj = 0           :: pos_integer(),  %% max length a object (byte)
          chunked_obj_len = 0          :: pos_integer(),  %% chunked object length for large object (byte)
          reading_chunked_obj_len = 0  :: pos_integer(),  %% reading chunked object length for large object (byte)
          threshold_of_chunk_len = 0   :: pos_integer()   %% threshold of chunk length for large object (byte)
         }).

-record(req_params, {
          %% basic info
          handler                    :: atom(),                 %% http-handler
          path = <<>>                :: binary(),               %% path (uri)
          bucket_name = <<>>         :: binary(),               %% bucket-name (for s3-api)
          bucket_info = undefined    :: term(),                 %% bucket (for s3-api)
          redundancy_method          :: atom(),                 %% redundancy method
          ec_method                  :: atom(),                 %% erasure-coding method
          ec_params                  :: {pos_integer(), pos_integer()}|undefined, %% erasure-coding params
          access_key_id = <<>>       :: binary(),               %% s3's access-key-id
          token_length = 0           :: non_neg_integer(),      %% length of tokened path
          min_layers = 0             :: non_neg_integer(),      %% acceptable # of min layers
          max_layers = 0             :: non_neg_integer(),      %% acceptable # of max layers
          custom_header_settings     :: list() | undefined,     %% http custom header settings
          timeout_for_header         :: pos_integer(),          %% Timeout for reading header
          timeout_for_body           :: pos_integer(),          %% Timeout for reading body
          sending_chunked_obj_len    :: pos_integer(),          %% sending chunk length
          qs_prefix = <<>>           :: binary() | none,        %% query string
          range_header               :: string(),               %% range header
          custom_metadata = <<>>     :: binary(),
          has_inner_cache = false    :: boolean(),              %% has inner-cache?
          has_disk_cache = false     :: boolean(),              %% has disk cache?
          is_cached = false          :: boolean(),              %% is cached?
          is_dir = false             :: boolean(),              %% is directory?
          is_location = false        :: boolean(),              %% is location?
          is_multi_delete = false    :: boolean(),              %% is multi delete request?
          %% for multipart upload
          dont_abort_cleanup = false :: boolean(),              %% whether removing related objects when handling abort MU request
          %% for large-object
          is_upload = false            :: boolean(),            %% is upload operation? (for multipart upload)
          is_aws_chunked = false       :: boolean(),            %% is AWS Chunked? (Signature V4)
          is_acl = false               :: boolean(),            %% is acl operation?
          ia_location = false          :: boolean(),            %% is location operation?
          upload_id = <<>>             :: binary(),             %% upload id for multipart upload
          upload_part_num = 0          :: non_neg_integer(),    %% upload part number for multipart upload
          max_chunked_objs = 0         :: non_neg_integer(),    %% max chunked objects
          max_len_for_multipart = 0    :: non_neg_integer(),    %% max length a multipart object (byte)
          max_len_of_obj = 0           :: non_neg_integer(),    %% max length a object (byte)
          chunked_obj_len = 0          :: non_neg_integer(),    %% chunked object length for large-object (byte)
          reading_chunked_obj_len = 0  :: non_neg_integer(),    %% creading hunked object length for large object (byte)
          threshold_of_chunk_len = 0   :: non_neg_integer(),    %% threshold of chunk length for large-object (byte)
          transfer_decode_fun       :: function() | undefined,  %% transfer decode function
          transfer_decode_state     :: #aws_chunk_decode_state{} | undefined,   %% transfer decode state
          %% For Latency Measurement
          begin_time = 0               :: non_neg_integer()     %% Handle Start Time
         }).

-record(put_req_params, {
          path = <<>> :: binary(),
          body = <<>> :: binary(),
          meta = <<>> :: binary(),
          msize = 0 :: non_neg_integer(),
          dsize = 0 :: non_neg_integer(),
          total_chunks = 0 :: non_neg_integer(),
          cindex = 0 :: non_neg_integer(),
          csize = 0 :: non_neg_integer(),
          digest = 0 :: non_neg_integer(),
          bucket_info = undefined :: term()|undefined
          }).

-record(transport_record, {
          transport :: module(),
          socket    :: inet:socket(),
          sending_chunked_obj_len :: pos_integer()
         }).

-record(cache, {
          etag         = 0    :: non_neg_integer(),   %% actual value is checksum
          mtime        = 0    :: non_neg_integer(),   %% gregorian_seconds
          content_type = <<>> :: binary() | string(), %% from a Content-Type header
          body         = <<>> :: binary(),            %% body (value),
          cmeta        = <<>> :: binary(),            %% custom metadata
          size         = 0    :: non_neg_integer(),   %% body size
          msize        = 0    :: non_neg_integer(),   %% custom metadata size
          file_path    = ""   :: file:name_all()      %% file path when this cache is stored on disk
         }).

-record(cache_condition, {
          expire          = 0  :: integer(),          %% specified per sec
          max_content_len = 0  :: integer(),          %% No cache if Content-Length of a response header was &gt this
          content_types   = [] :: list() | undefined, %% like ["image/png", "image/gif", "image/jpeg"]
          path_patterns   = [] :: list() | undefined, %% compiled regular expressions
          sending_chunked_obj_len = 0 :: pos_integer()%% sending chunk length
         }).
