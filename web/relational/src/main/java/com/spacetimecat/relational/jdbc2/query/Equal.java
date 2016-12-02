package com.spacetimecat.relational.jdbc2.query;

public final class Equal extends AbstractPredicate2
{
    private final String left;
    private final String right;

    public Equal (String left, String right)
    {
        this.left = left;
        this.right = right;
    }

    @Override
    public Predicate2 not ()
    {
        return null;
    }

    @Override
    public Predicate2 or (Predicate2 that)
    {
        return null;
    }

    @Override
    public String toSqlExpression ()
    {
        return String.format("(%s) = (%s)", left, right);
    }
}
