package com.spacetimecat.relational.jdbc2.query;

abstract class AbstractTable implements Table
{
    @Override
    public final Table join (Table that)
    {
        return new Join(this, that);
    }

    @Override
    public final Table filter (Predicate2 predicate)
    {
        if (predicate == null) { throw new NullPointerException("predicate"); }
        return new Filter(this, predicate);
    }
}
