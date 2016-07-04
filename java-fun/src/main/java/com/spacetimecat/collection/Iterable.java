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
    , Foldable<A>
    , IntegerIndexed<A>
    , Mappable
{
    @Override
    Iterator<A> iterator ();

    Iterable<A> append (BasicIterable<? extends A> that);

    /**
     * Call the procedure for each element.
     *
     * @param f procedure taking an element
     *
     * @return this
     */
    Iterable<A> forEach (BasicProcedure1<A> f);

    <B> Iterable<B> map (BasicFunction1<? super A, B> f);

    <B> Iterable<B> flatMap (BasicFunction1<? super A, BasicIterable<B>> f);

    <K, V> BasicDumpableMap<K, V> mapToBasicDumpableMap (BasicFunction1<? super A, Tuple2<K, V>> f);

    <B, C> Iterable<C> zip (BasicIterable<B> bs, BasicFunction2<A, B, C> f);

    @Override
    Iterable<A> filter (BasicPredicate1<? super A> p);

    @Override
    Iterable<A> dumpTo (java.util.Collection<? super A> target);

    boolean isEmpty ();

    A findAny (BasicPredicate1<? super A> p);
}
