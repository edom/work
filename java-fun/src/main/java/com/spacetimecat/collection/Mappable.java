package com.spacetimecat.collection;

import com.spacetimecat.function.BasicFunction1;

/**
 * <p>If {@code C<A>} implements {@link Mappable},
 * then {@code C<A>} should have a method with the signature
 * {@code <B> C<B> map (BasicFunction1<A, B>)}.</p>
 *
 * <p>If {@code C<A>} implements {@link Mappable},
 * then {@code C<A>} may also have a method with the signature
 * {@code <B> C<B> mapEager (BasicFunction1<A, B>)}.</p>
 *
 * <p>This aspires to be the Functor type class in Haskell,
 * but the type system of Java is not expressive enough for this,
 * so {@link Mappable} is just a marker interface,
 * and we depend on the programmers to honor the convention.</p>
 *
 * @see BasicFunction1
 */
public interface Mappable
{
}
