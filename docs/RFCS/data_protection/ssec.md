# Server Side Object Encryption with Customer-Provided Encryption Keys *(SSE-C)*

- Category: Protecting Data in LeoFS
- Status: in-progress
- Start Date: 2018-04-18
- Authors: [Yosuke Hara](https://github.com/yosukehara)
- LeoFS Issue: [#114](https://github.com/leo-project/leofs/issues/114)


## Summary

This RFC suggests implementing server-side object encryption with customer-provided encryption Keys, *SSE-C* which is one way of protecting data using encryption and conform to AWS S3-API - Protecting Data Using SSE-C[^1].


## Motivation

In order to improve the data protection feature and to provide LeoFS as an enterprise secure data storage, SSE-C must be implemented as one of the data protection functions. After we implement this function, we can provide LeoFS for GDPR[^2], FHIR[^3] and other data security needs.


## Detailed design
### Prerequisites

The data path between LeoFS' clients and LeoGateway is securely protected by HTTPS and the internal communication between LeoGateway and LeoStorage is also protected by Erlang distribution over TLS[^4].

LeoFS never store **keys of all customers** and the **original checksum** and **size** of an object on metadata. Since we suggest the requirement that *"Anyone who has access to the stored data can try to manipulate the data - even if the data is encrypted. (Quoted from 'Go implementation of the Data At Rest Encryption (DARE) format[^5])"*, storing the original checksum (MD5) and the size could probably allow attackers to derive the original content by brute force attack based on the size and checksum.


### Requirements

LeoStorage encrypts and decrypts objects because LeoStorage's CPU usage tends to be considerably lower than LeoGateway. Furthermore, in many cases, the number of LeoStorage's in a LeoFS storage system is larger than the number of LeoGateway and thus, load balancing can be expected.

In each of the following operations, LeoStorage handles encryption and decryption of an object after an operation is requested from LeoGateway.

* GET
* Range GET
* HEAD
* PUT
* Multipart Upload
* COPY

LeoGateway never caches objects to securely protect.


### Request Handling on LeoGateway

When LeoGateway receives an SSE-C request from a LeoFS' client, it retrieves the encryption key information from the request header, then it sends the information and the object *(only if PUT or Multipart Upload operation was requested)* to LeoStorage.

When a requested object corresponds to Large Size Object, LeoGateway divides the object to plural chunks of it, then it sends each chunk with the encryption key information to LeoStorage in order to individually execute object encryption.


#### Encryption key information in HTTP Header:

| Name  | Description  |
|----|----|
|`x-amz-server-side​-encryption​-customer-algorithm`| Encryption algorithm *(AES 256)* |
|`x-amz-server-side​-encryption​-customer-key` | 256-bit, base64-encoded encryption key |
| `x-amz-server-side​-encryption​-customer-key-MD5` | Base64-encoded 128-bit MD5 digest of the encryption key |


#### COPY Operation

There are four patterns in COPY operation. In SSE-C implementation, LeoFS needs to implement the following cases 1, 2 and 3:

| No  | Source      | Destination | Status |
|:---:|-------------|-------------|--------|
| 1   | Encrypted   | Encrypted   | Plans to implement with v2.0 |
| 2   | Encrypted   | Unencrypted | Plans to implement with v2.0 |
| 3   | Unencrypted | Encrypted   | Plans to implement with v2.0 |
| 4   | Unencrypted | Unencrypted | Ready  |


If the source object is encrypted using SSE-C *(case 1 and 2)*, LeoGateway receives encryption key information which includes the following headers so that LeoStorage can decrypt the object for copying.

| Name  | Description  |
|----|----|
|`x-amz-copy-source​-server-side​-encryption​-customer-algorithm`| Encryption algorithm *(AES 256)* |
|`x-amz-copy-source​-server-side​-encryption​-customer-key` | 256-bit, base64-encoded encryption key |
| `x-amz-copy-source-​server-side​-encryption​-customer-key-MD5` | Base64-encoded 128-bit MD5 digest of the encryption key |

Furthermore, when encrypting the destination object *(case 1 and 3)*, LeoGateway receives encryption key information which is [Encryption key information in HTTP Header](#encryption-key-information-in-http-header).


### Encryption and decryption of an object on LeoStorage

LeoStorage receives an object and an encryption key information when LeoGateway requests **WRITE operation**, then it creates an initialization vector *(IV)* which is commonly called a nonce *(number used once)*, then it encrypts the object based on **the encryption algorithm** and **the encryption key** and **the initialization vector**, finally it replicates and stores the encrypted object and metadata which includes the following items:

* 256-bit, Key Encryption Key
* Initialization Vector *(Nonce)*

A needle *(LeoObjectStorage Data Format)* includes those items which are **stored in the custom-metadata section** in order to be able to transform metadata to each other for durability. See [LeoFS Documentation / Architecture / LeoStorage - Data Structure](https://leo-project.net/leofs/docs/architecture/leo_storage/#data-structure).

LeoStorage receives an encryption key information when LeoGateway requests **READ operation**, then it looks up the metadata which includes the encryption information and the object, then the object is decrypted based on **the encryption algorithm**, **the encryption key**, and **the initialization vector**, finally, it returns the decrypted object to LeoGateway.

Even when Range GET operation is requested, LeoStorage handles the same as GET operation and retrieves the range of the object after object decryption.


## Drawbacks

* Compared with an unencrypted object, LeoFS certainly degrades the read performance when retrieving an encrypted object *(secure data)* because LeoGateway never caches the objects to securely protect.


## Rationale and Alternatives

Initially, we were considering **implementing object encryption, and decryption on LeoGateway**. However, we realized that there are the following problems, we decided to implement that on LeoStorage.

* LeoGateway's CPU usage tends to be high. We must not assign more processing, object encryption and decryption to LeoGateway.
* In many cases, the number of LeoGateway is smaller than the number of LeoStorage. Load balancing cannot be expected.
* In order to prevent vulnerabilities, secure data must not be cached.
* When handling Range GET operation, LeoGateway retrieves an object in the specified range from LeoStorage, then decrypt it, finally returns it to a client. In this way, the data traffic is higher than retrieving unencrypted objects.


## Unresolved questions


[^1]: <a href="https://docs.aws.amazon.com/AmazonS3/latest/dev/ServerSideEncryptionCustomerKeys.html" target="_blank">Amazon S3 / Protecting Data Using Server-Side Encryption with Customer-Provided Encryption Keys *(SSE-C)*</a>
[^2]: <a href="https://en.wikipedia.org/wiki/General_Data_Protection_Regulation" target="_blank">GDPR *(General Data Protection Regulation)*</a>
[^3]: <a href="https://en.wikipedia.org/wiki/Fast_Healthcare_Interoperability_Resources" target="_blank">FHIR *(Fast Healthcare Interoperability Resources)*</a>
[^4]: <a href="https://www.erlang-solutions.com/blog/erlang-distribution-over-tls.html" target="_blank">Erlang distribution over TLS</a>
[^5]: <a href="https://github.com/minio/sio" target="_blank">Secure IO / Go implementation of the Data At Rest Encryption *(DARE)* format</a>