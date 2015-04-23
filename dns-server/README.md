Forward DNS queries to web services.

Only a small part of DNS.

No recursion.

No security.

No encryption.

No caching.

No multi-processing.

Do not use for critical applications.

Should only listen on localhost.

# Usage

```
dns-server

    --port PORT
        Listen on the given UDP port on localhost.
        Defaults to 1053 if not given.

    --answer-min-ttl VAL
        Replace the TTL of an answer record with VAL if that TTL is lower than VAL.
```
