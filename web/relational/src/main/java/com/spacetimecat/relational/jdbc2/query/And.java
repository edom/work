package com.spacetimecat.relational.jdbc2.query;

public class And extends AbstractPredicate2
{
    private final Predicate2 left;
    private final Predicate2 right;

    public And (Predicate2 left, Predicate2 right)
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
        return String.format("(%s) AND (%s)", left.toSqlExpression(), right.toSqlExpression());
    }
}
