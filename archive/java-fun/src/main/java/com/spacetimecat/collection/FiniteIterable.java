package com.spacetimecat.collection;

import com.spacetimecat.function.BasicFunction1;
import com.spacetimecat.function.BasicPredicate1;

/**
 * <p>An {@link Iterable} that will end.</p>
 *
 * @param <A> element type
 */
public interface FiniteIterable<A> extends
    BasicFiniteIterable<A>
    , Iterable<A>
    , Sized
{
    @Override
    FiniteIterator<A> iterator ();

    @Override
    Iterable<A> append (BasicIterable<? extends A> that);

    FiniteIterable<A> append (BasicFiniteIterable<? extends A> that);

    @Override
    <B> FiniteIterable<B> map (BasicFunction1<? super A, B> f);

    <B> FiniteIterable<B> flatMapFinite (BasicFunction1<? super A, BasicFiniteIterable<B>> f);

    @Override
    FiniteIterable<A> filter (BasicPredicate1<? super A> p);
}
