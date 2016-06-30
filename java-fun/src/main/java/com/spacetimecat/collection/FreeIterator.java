package com.spacetimecat.collection;

import com.spacetimecat.function.Function1;
import com.spacetimecat.function.Function2;
import com.spacetimecat.function.Procedure1;

/**
 * <p>If you have a {@link BasicIterator},
 * this gives you an {@link Iterator} for free.</p>
 *
 * <p>This also gives you a {@link Foldable} for free,
 * although this can only be used one time because folding has the side-effect of
 * moving the iterator to the end of the iterable.</p>
 *
 * <p>This can also wrap the standard Iterator. See {@link #from(java.util.Iterator)}.</p>
 *
 * @param <A>
 */
public final class FreeIterator<A> implements Iterator<A>
{
    private final BasicIterator<A> bi;

    public FreeIterator (BasicIterator<A> bi)
    {
        this.bi = bi;
    }

    @Override
    public Iterator<A> skip (int n)
    {
        for (int i = 0; i < n; ++i)
        {
            next();
        }
        return this;
    }

    @Override
    public Iterator<A> forEach (Procedure1<A> f)
    {
        A a;
        while ((a = bi.next()) != null)
        {
            f.call(a);
        }
        return this;
    }

    @Override
    public <B> Iterator<B> map (Function1<A, B> f)
    {
        return new FreeIterator<>(new MappedBasicIterator<>(this, f));
    }

    @Override
    public <B, C> Iterator<C> zip (BasicIterator<B> that, Function2<A, B, C> f)
    {
        return new FreeIterator<>(new ZipBasicIterator<>(this, that, f));
    }

    @Override
    public A next ()
    {
        return bi.next();
    }

    @Override
    public <B> B fold (B e, Function2<B, A, B> f)
    {
        B r = e;
        A a;
        while ((a = bi.next()) != null)
        {
            r = f.call(r, a);
        }
        return r;
    }

    public static <A> FreeIterator<A> from (java.util.Iterator<A> i)
    {
        return new FreeIterator<>(new StandardBasicIterator<>(i));
    }
}
