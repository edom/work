:- module(database, [
]).
:- reexport('./database_internal_dcg.pro', [
    type_bytecount/2
]).
:- reexport('./database_internal_transput.pro', [
    db_open_file/3,
    db_close/1,
    db_flush/1,
    db_seek/2,
    db_read/2,
    db_write/2
]).
/** <module> making databases

## Usage

There are several ways to use this library:
    - data exchange format, similar to ASN.1 or Protobuf
    - low-level database library, similar to the layer below Berkeley DB

Low-level fixed-length-record single-threaded interface:
    - Use setup_call_cleanup/3.
    - Important: these predicates are not thread-safe.
    Do not use from multiple threads.
    - Open the database with db_open_file/3.
    - Move around with db_seek/2.
    - Read the database with db_read/2.
    - Write the database with db_write/2.
    - Close the database with db_close/1.
    - Flush the database with db_flush/1.
    Note that this does _not_ guarantee that the data has been written to disk.

```
setup_call_cleanup(
    db_open_file(Path, Type, Handle)
    , do_something_with(Handle)
    , db_close(Handle)
)

db_number_record(Db, 0, R)
```

Definite-clause grammar:
    - The meaning of types and their bit layout is described in the DCG rule type_value//2.
    - type_bytecount/2 calculates the number of bytes occupied by an instance of a type.
    - type_value_bytes/3 is a convenience predicate.

Internals and dependencies:
    - database_internal_dcg.pro
    - database_internal_pure.pro
    - database_internal_transput.pro
    - transput.pro

## Design

We begin with fixed-length records.
    - pros
        - simple to seek to the record number N
    - cons
        - limited length
        - padding wastes storage

Storing tagged unions
    - tag1([0-T0, 1-T1, ..., N-TN]) where N < 256
    - nullable(T) = tag1([0-[], 1-T])

Long way:
    - data integrity: faulty storage, tampering
        - no need fancy things; just replicate; you need backup anyway

## Ramblings

Database and application should not be separate.

Database should back up, replicate, and restore itself across several nodes.

Code is smaller than data.
It is easier to move code than to move data.

Similar things:
    - This library works at a layer below Berkeley DB:
        - Unfortunately Oracle owns Berkeley DB. https://en.wikipedia.org/wiki/Berkeley_DB
        - Alternatives to Berkeley DB? https://stackoverflow.com/questions/260804/alternative-to-berkeleydb
    - https://en.wikipedia.org/wiki/Database_Management_Library
        - Unfortunately it's C++.
    - "cdb acts as an on-disk associative array" https://en.wikipedia.org/wiki/Cdb_(software)
    - https://en.wikipedia.org/wiki/Embedded_database

Datomic seems cool.
What can we learn from it?
    - Replication is simplified by append-only / log-structured data / inherent versioning.

## Issues

    - type_value//2
        - What is Unicode equivalence? How should we compare strings?
        - Which UTF-8 canonicalization should we use for storing character strings in the database?
            - http://www.swi-prolog.org/pldoc/man?section=unicode
        - Should we just store bytes, and let the application programmer be resposible for the encoding?
    - Performance?
    We assume that there is a Prolog compiler smart enough to optimize a Prolog integer list into a C byte array.
    - How do we ensure that returning from db_flush/1 implies that the data is in the disk?
    We don't.
    There are too many uncontrollable caches between the RAM and the hard disk surface.
    Instead, run at least 3 processes at different machines at different locations,
    periodically back up data to external storage,
    periodically test restoring the backups.
    - thread-safety
    - indexes
    - sequences
*/
