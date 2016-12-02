package com.spacetimecat.relational.jdbc2.query;

public interface Predicate2
{
    Predicate2 not ();

    Predicate2 and (Predicate2 that);

    Predicate2 or (Predicate2 that);

    String toSqlExpression ();
}
