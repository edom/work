package com.spacetimecat.collection;

import com.spacetimecat.function.*;

/**
 * <p>Make {@link Iterator}s.</p>
 *
 * <p>An iterable can be thought as a lazy list.</p>
 *
 * @param <A> element type
 */
public interface Iterable<A> extends
    BasicIterable<A>
    , Dumpable<A>
    , ToNewStdList<A>
    , Filterable<A>
    , FindAny<A>
    , Foldable<A>
    , ForEach<A>
    , IntegerIndexed<A>
    , Mappable<A>
{
    @Override
    Iterator<A> iterator ();

    Iterable<A> append (BasicIterable<? extends A> that);

    @Override
    Iterable<A> forEach (BasicProcedure1<? super A> f);

    @Override
    <B> Iterable<B> map (BasicFunction1<? super A, B> f);

    <B> Iterable<B> flatMap (BasicFunction1<? super A, BasicIterable<B>> f);

    <K, V> BasicDumpableMap<K, V> mapToBasicDumpableMap (BasicFunction1<? super A, Tuple2<K, V>> f);

    <B, C> Iterable<C> zip (BasicIterable<B> bs, BasicFunction2<A, B, C> f);

    @Override
    Iterable<A> filter (BasicPredicate1<? super A> p);

    @Override
    Iterable<A> dumpTo (java.util.Collection<? super A> target);

    boolean isEmpty ();

    /**
     * <p>Compute every elements right now.</p>
     * <p>Do not call this if the iterable is very large or infinite.</p>
     * @return a view
     */
    FiniteIterable<A> eager ();
}
