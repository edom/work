% ------- web application

thing_property(database,Id,Key,Value) :- database(Id,Props), member(Key-Value,Props).
thing_property(table,Id,Key,Value) :- table(Id,Props), member(Key-Value,Props).

/*
sudo -u postgres psql
    CREATE USER test PASSWORD 'test';
    CREATE DATABASE test OWNER test;
*/
database(accounting,[
    name-accounting
    , host-localhost
    , port-5432
    , catalog-test
    , username-test
    , password-test
]).
database_property(D,Key,Value) :- thing_property(database,D,Key,Value).
database(D) :- database(D,_).
database_name(A,B) :- database_property(A,name,B).
database_host(A,B) :- database_property(A,host,B).
database_port(A,B) :- database_property(A,port,B).
database_catalog(A,B) :- database_property(A,catalog,B).
database_username(A,B) :- database_property(A,username,B).
database_password(A,B) :- database_property(A,password,B).

table(account,[
    database-accounting
    , schema-account
    , name-account
]).

table(A) :- table(A,_).
table_database(A,B) :- thing_property(table,A,database,B).
table_schema(A,B) :- thing_property(table,A,schema,B).
table_name(A,B) :- thing_property(table,A,name,B).

/** page(?PageId,?Name,?HttpMethod,?Path,?PageOpts,?Content) is nondet.

A convenience predicate for specifying pages.
*/
page(get-home, home, 'GET', '/', [], [
    let([value = request_parameter(value)],[
        "The last value was ", state(lastvalue),
        state(lastvalue) := value
    ])
]).

/*
We could define our own operators, but should we?

Pro: The code looks pretty.

Con: The syntax error message will always be "Syntax error: Operator priority clash".

:- op(1140,xfx,limit).
:- op(1130,xfx,order_by).
:- op(1120,xfx,where).
:- op(1110,yfx,join).
% :- op(700,yfx,as).
:- write_canonical(
    $table1 as t1 join $table2 as t2 join $table3 as t3
    where [t1:field1=t2:field2, t2:field2=t3:field3]
    order_by [t1:field1, t2:field2]
    limit 100
).
*/

page(get-help, help, 'GET', '/help', [], [
    "Help"
    , query(limit(15,order_by([id,name],$account)))
]).

page(post-help, help, 'POST', '/help', [], [
    "Help (POST)"
]).

% ------- wiring

page(P) :- page(P,_,_,_,_,_).
page_name(P,N) :- page(P,N,_,_,_,_).
page_method(P,M) :- page(P,_,M,_,_,_).
page_path(P,A) :- page(P,_,_,A,_,_).
page_options(P,A) :- page(P,_,_,_,A,_).
page_content(P,C) :- page(P,_,_,_,_,C).
page_option(P,A) :- page_options(P,L), member(A,L).
