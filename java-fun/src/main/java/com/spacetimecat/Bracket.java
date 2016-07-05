package com.spacetimecat;

import com.spacetimecat.function.BasicFunction1;
import com.spacetimecat.function.BasicProcedure1;

/**
 * <p>Functional-style try-with-resources.</p>
 */
public interface Bracket<R>
{
    R withProcedure (BasicProcedure1<? super R> f);

    /**
     * <p>Call f with this, and clean up after f returns, even if f throws an exception.</p>
     * @param f will be passed {@code this}
     * @param <B> the type of the return value
     * @return what f returns
     */
    <B> B withFunction (BasicFunction1<? super R, B> f);
}
