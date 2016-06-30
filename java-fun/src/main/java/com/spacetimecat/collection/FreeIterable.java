package com.spacetimecat.collection;

import com.spacetimecat.function.Function1;
import com.spacetimecat.function.Function2;
import com.spacetimecat.function.Procedure1;

import java.util.Arrays;

/**
 * <p>If you have a {@link BasicIterable}, this gives you an {@link Iterable} for free.</p>
 * @param <A>
 */
public final class FreeIterable<A> implements Iterable<A>
{
    private final BasicIterable<A> bi;

    public FreeIterable (BasicIterable<A> bi)
    {
        this.bi = bi;
    }

    public static <A> FreeIterable<A> from (java.lang.Iterable<A> i)
    {
        return new FreeIterable(new StandardBasicIterable(i));
    }

    public static <A> FreeIterable<A> from (A... as)
    {
        return from(Arrays.asList(as));
    }

    @Override
    public Iterable<A> forEach (Procedure1<A> f)
    {
        new FreeIterator<>(bi.iterator()).forEach(f);
        return this;
    }

    @Override
    public <B> Iterable<B> map (Function1<A, B> f)
    {
        return new FreeIterable<>(new MappedBasicIterable<>(this, f));
    }

    @Override
    public <B, C> Iterable<C> zip (BasicIterable<B> bs, Function2<A, B, C> f)
    {
        return new FreeIterable<>(new ZipBasicIterable<>(this, bs, f));
    }

    @Override
    public Iterator<A> iterator ()
    {
        return new FreeIterator<>(bi.iterator());
    }

    @Override
    public <B> B fold (B e, Function2<B, A, B> f)
    {
        return new FreeIterator<>(bi.iterator()).fold(e, f);
    }
}
