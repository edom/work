package com.spacetimecat.concurrent.batch.internal.hash;

import java.util.List;
import java.util.stream.Collectors;

/**
 * <p>
 *     Something with separately defined
 *     {@link Object#hashCode() hashCode}
 *     and {@link Object#equals(Object) equals}.
 * </p>
 *
 * @param <A>
 * wrapped type
 */
public final class CustomHashed<A>
{
    private final Equivalence<A> equivalence;
    private final Hashing<A> hashing;
    private final A delegate;

    public CustomHashed (Equivalence<A> equivalence, Hashing<A> hashing, A delegate)
    {
        if (equivalence == null) { throw new NullPointerException("equivalence"); }
        if (hashing == null) { throw new NullPointerException("hashing"); }
        if (delegate == null) { throw new NullPointerException("delegate"); }
        this.equivalence = equivalence;
        this.hashing = hashing;
        this.delegate = delegate;
    }

    @Override
    public int hashCode ()
    {
        return hashing.hashCodeOf(delegate);
    }

    @SuppressWarnings("unchecked")
    @Override
    public boolean equals (Object that_)
    {
        if (this == that_) { return true; }
        if (!(that_ instanceof CustomHashed)) { return false; }
        final CustomHashed that = (CustomHashed) that_;
        try
        {
            return equivalence.areEquivalent(this.delegate, (A) that.delegate);
        }
        catch (ClassCastException e)
        {
            return false;
        }
    }

    @Override
    public String toString ()
    {
        return delegate.toString();
    }

    public A unwrap ()
    {
        return delegate;
    }

    public static <A> List<A> unwrapList (List<CustomHashed<A>> list)
    {
        return list.stream().map(CustomHashed::unwrap).collect(Collectors.toList());
    }
}
