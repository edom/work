package com.spacetimecat.collection;

import com.spacetimecat.function.BasicFunction1;
import com.spacetimecat.function.BasicFunction2;
import com.spacetimecat.function.BasicPredicate1;
import com.spacetimecat.function.BasicProcedure1;

/**
 * <p>If you have a {@link BasicIterable}, this gives you an {@link Iterable} for free.</p>
 * @param <A> element type
 */
final class FreeIterable<A> implements Iterable<A>
{
    private final BasicIterable<A> bi;

    FreeIterable (BasicIterable<A> bi)
    {
        this.bi = bi;
    }

    @Override
    public Iterable<A> forEach (BasicProcedure1<A> f)
    {
        iterator().forEach(f);
        return this;
    }

    @Override
    public <B> Iterable<B> map (BasicFunction1<A, B> f)
    {
        return new FreeIterable<>(new MappedBasicIterable<>(bi, f));
    }

    @Override
    public <B> Iterable<B> flatMap (BasicFunction1<? super A, BasicIterable<B>> f)
    {
        return new FreeIterable<>(new BasicIterableFlattening<>(new MappedBasicIterable<>(bi, f)));
    }

    @Override
    public <B, C> Iterable<C> zip (BasicIterable<B> bs, BasicFunction2<A, B, C> f)
    {
        return new FreeIterable<>(new ZipBasicIterable<>(bi, bs, f));
    }

    @Override
    public Iterable<A> filter (BasicPredicate1<? super A> p)
    {
        return new FreeIterable<>(new FilteredBasicIterable<>(bi, p));
    }

    @Override
    public Iterable<A> dumpTo (java.util.Collection<? super A> target)
    {
        iterator().dumpTo(target);
        return this;
    }

    @Override
    public boolean isEmpty ()
    {
        return iterator().next() == null;
    }

    @Override
    public Iterator<A> iterator ()
    {
        return new FreeIterator<>(bi.iterator());
    }

    @Override
    public Iterable<A> append (BasicIterable<? extends A> that)
    {
        return new FreeIterable<>(new BasicIterableAppending<>(bi, that));
    }

    @Override
    public <B> B fold (B e, BasicFunction2<B, A, B> f)
    {
        return iterator().fold(e, f);
    }

    @Override
    public A at (int i)
    {
        return iterator().at(i);
    }

    @Override
    public A at (Integer i)
    {
        return at(i.intValue());
    }

    @Override
    public java.util.List<A> toNewStdList ()
    {
        return iterator().toNewStdList();
    }
}
