package com.spacetimecat.collection;

import com.spacetimecat.function.Function1;
import com.spacetimecat.function.Function2;
import com.spacetimecat.function.Procedure1;

import java.util.ArrayList;

/**
 * <p>If you have an {@link BasicIterable}, this gives you a {@link List} for free.</p>
 */
public final class FreeList<A> implements List<A>
{
    private final BasicIterable<A> bi;

    public FreeList (BasicIterable<A> bi)
    {
        this.bi = bi;
    }

    @Override
    public A at (int i)
    {
        return new FreeIterator<>(bi.iterator()).skip(i).next();
    }

    @Override
    public List<A> forEach (Procedure1<A> f)
    {
        new FreeIterable<>(bi).forEach(f);
        return this;
    }

    @Override
    public <B, C> List<C> zip (BasicIterable<B> bs, Function2<A, B, C> f)
    {
        return new FreeList<>(new ZipBasicIterable<>(bi, bs, f));
    }

    @Override
    public <B> List<B> map (Function1<A, B> f)
    {
        return new FreeList<>(new MappedBasicIterable<>(bi, f));
    }

    @Override
    public java.util.List<A> copyStdList ()
    {
        final java.util.List<A> result = new ArrayList<>();
        new FreeIterator<>(bi.iterator()).forEach(new Procedure1<A>()
        {
            @Override
            public void call (A a)
            {
                result.add(a);
            }
        });
        return result;
    }

    @Override
    public <B> B fold (B e, Function2<B, A, B> f)
    {
        return new FreeIterator<>(bi.iterator()).fold(e, f);
    }

    @Override
    public BasicIterator<A> iterator ()
    {
        return bi.iterator();
    }
}
