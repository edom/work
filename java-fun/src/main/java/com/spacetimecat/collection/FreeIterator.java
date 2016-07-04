package com.spacetimecat.collection;

import com.spacetimecat.function.BasicFunction1;
import com.spacetimecat.function.BasicFunction2;
import com.spacetimecat.function.BasicPredicate1;
import com.spacetimecat.function.BasicProcedure1;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;

/**
 * <p>If you have a {@link BasicIterator},
 * this gives you an {@link Iterator} for free.</p>
 *
 * <p>This also gives you a {@link Foldable} for free,
 * although this can only be used one time because folding has the side-effect of
 * moving the iterator to the end of the iterable.</p>
 */
class FreeIterator<A> implements Iterator<A>
{
    private final BasicIterator<A> bi;

    FreeIterator (BasicIterator<A> bi)
    {
        this.bi = bi;
    }

    @Override
    public boolean all (BasicPredicate1<A> p)
    {
        boolean b = true;
        A a;
        do { a = bi.next(); }
        while (a != null && (b &= p.at(a)));
        return b;
    }

    @Override
    public boolean any (BasicPredicate1<A> p)
    {
        boolean b = false;
        A a;
        do { a = bi.next(); }
        while (a != null && !(b |= p.at(a)));
        return b;
    }

    @Override
    public Iterator<A> append (BasicIterator<A> that)
    {
        return new FreeIterator<>(new BasicIteratorAppending<>(bi, that));
    }

    @Override
    public Iterator<A> distinct ()
    {
        return new FreeIterator<>(new BasicIteratorDistinct<>(bi));
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
    public Iterator<A> forEach (BasicProcedure1<A> f)
    {
        A a;
        while ((a = bi.next()) != null)
        {
            f.call(a);
        }
        return this;
    }

    @Override
    public <B> Iterator<B> map (BasicFunction1<A, B> f)
    {
        return new FreeIterator<>(new MappedBasicIterator<>(this, f));
    }

    @Override
    public <B, C> Iterator<C> zip (BasicIterator<B> that, BasicFunction2<A, B, C> f)
    {
        return new FreeIterator<>(new ZipBasicIterator<>(this, that, f));
    }

    @Override
    public Iterator<A> filter (BasicPredicate1<? super A> p)
    {
        return new FreeIterator<>(new FilteredBasicIterator<>(bi, p));
    }

    @Override
    public Iterator<A> dumpTo (java.util.Collection<? super A> target)
    {
        return forEach(target::add);
    }

    @Override
    public A last ()
    {
        A a = null;
        A b;
        while ((b = bi.next()) != null)
        {
            a = b;
        }
        return a;
    }

    @Override
    public <B> Iterator<B> mapEager (BasicFunction1<A, B> f)
    {
        final List<B> list = map(f).toNewStdList();
        return new FreeIterator<>(new BasicIteratorFromJavaUtil<>(list.iterator()));
    }

    @Override
    public <B> CallableIterator<B> mapToCallable (BasicFunction1<A, Callable<B>> f)
    {
        return new FreeCallableIterator<>(map(f));
    }

    @Override
    public A next ()
    {
        return bi.next();
    }

    @Override
    public <B> B fold (B e, BasicFunction2<B, A, B> f)
    {
        return scan(e, f).last();
    }

    @Override
    public A at (int i)
    {
        return skip(i).next();
    }

    @Override
    public A at (Integer i)
    {
        return at(i.intValue());
    }

    @Override
    public java.util.List<A> toNewStdList ()
    {
        final ArrayList<A> result = new ArrayList<>();
        dumpTo(result);
        return result;
    }

    @Override
    public <B> Iterator<B> scan (B e, BasicFunction2<B, ? super A, B> f)
    {
        return new FreeIterator<>(new ScanningBasicIterator<>(e, f, bi));
    }
}
