package com.spacetimecat.relational.jdbc2.query;

public abstract class AbstractPredicate2 implements Predicate2
{
    @Override
    public final Predicate2 and (Predicate2 that)
    {
        return new And(this, that);
    }
}
