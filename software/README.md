# Some software

## Contents

[`boot`](boot/) is about tailoring SWI-Prolog 7.6.4,
mostly about module system and expansion system.
These files should be loaded with consult/1, not with use_module/2.

- [data.pro](data.pro): describe all data?
- [database.pro](database.pro): making databases
- [enterprise.pro](enterprise.pro): Prolog for enterprise applications?
- [haskell.pro](haskell.pro): Haskell?
- [jvm_cls.pro](jvm_cls.pro): Java class file?
- [lambda_calculus_normal.pro](lambda_calculus_normal.pro): lambda calculus with normal-order reduction strategy
- [odbc_postgresql.pro](odbc_postgresql.pro): work with PostgreSQL via ODBC?
- [prolog_translate.pro](prolog_translate.pro): Prolog program analysis?
- [ps1_bit.pro](ps1_bit.pro): bit manipulation
- [ps1_decompile.pro](ps1_decompile.pro): PlayStation 1 decompiler?
- [profon.pro](profon.pro): PROgrammation en FONctions, a functional companion to Prolog?
- [test.pro](test.pro): Tests for some sketches

## How to run

```
./prolog.sh FILE ...
```

If you don't know what to run, you may want to run test_all/0 in `test.pro`.

The file `boot/load.pro` provides `consult_unregistered/1` and other things.

## How to view documentation

Run this:

```
swipl_opts='--pldoc=4002' ./prolog.sh -- FILE ...
```

Open http://localhost:4002/pldoc/

## To-do

- SQL DDL escaping
