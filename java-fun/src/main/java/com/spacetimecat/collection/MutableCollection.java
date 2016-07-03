package com.spacetimecat.collection;

public interface MutableCollection<A> extends
    BasicMutableCollection<A>
    , HasImmutableFacade<A>
{
    @Override
    MutableCollection<A> add (A a);

    MutableCollection<A> addAll (java.util.Collection<A> that);
}
