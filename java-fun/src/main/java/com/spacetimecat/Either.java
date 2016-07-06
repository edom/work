package com.spacetimecat;

import com.spacetimecat.collection.Mappable;
import com.spacetimecat.function.BasicFunction1;

/**
 * <p>Sum type.</p>
 * @param <A> left type
 * @param <B> right type
 */
public interface Either<A, B> extends Mappable<B>
{
    <C> C either (BasicFunction1<? super A, C> f, BasicFunction1<? super B, C> g);

    <C, D> Either<C, D> mapBoth (BasicFunction1<? super A, C> f, BasicFunction1<? super B, D> g);

    <C> Either<C, B> mapLeft (BasicFunction1<? super A, C> f);

    <C> Either<A, C> mapRight (BasicFunction1<? super B, C> g);

    <C> Either<A, C> bind (BasicFunction1<? super B, Either<A, C>> k);

    @Override
    <C> Either<A, C> map (BasicFunction1<? super B, C> f);
}
