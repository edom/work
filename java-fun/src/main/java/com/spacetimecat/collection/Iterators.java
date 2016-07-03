package com.spacetimecat.collection;

import com.spacetimecat.Limbo;
import com.spacetimecat.Limbos;
import com.spacetimecat.objmap.BasicUnpackRow;

import java.sql.ResultSet;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;

/**
 * <p>Make lazy iterators.</p>
 */
public final class Iterators
{
    private Iterators () {}

    public static <A> Iterator<A> from (BasicIterator<A> i)
    {
        if (i == null) { throw new NullPointerException(); }
        if (i instanceof Iterator) { return (Iterator<A>) i; }
        return new FreeIterator<>(i);
    }

    public static <A> Iterator<A> from (java.util.Iterator<A> i)
    {
        if (i == null) { throw new NullPointerException(); }
        return new FreeIterator<>(new BasicIteratorFromJavaUtil<>(i));
    }

    public static <A> RowIterator<A> from (BasicUnpackRow<A> bur, ResultSet rs)
    {
        final Limbo m = Limbos.open().add(rs);
        return new FreeRowIterator<>(new BasicIteratorFromJdbc<>(bur, m, rs));
    }

    public static <A> RowIterator<A> from (BasicUnpackRow<A> rd, Limbo m, ResultSet rs)
    {
        return new FreeRowIterator<>(new BasicIteratorFromJdbc<>(rd, m, rs));
    }

    public static <A> BasicIterator<A> parallelize (ExecutorService es, BasicIterator<Callable<A>> bi)
    {
        return new ImplicitFutureIterator<>(new FreeIterator<>(bi).map(es::submit));
    }
}
