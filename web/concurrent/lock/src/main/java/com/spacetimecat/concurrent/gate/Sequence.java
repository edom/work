package com.spacetimecat.concurrent.gate;

import java.util.Arrays;
import java.util.Collection;

/**
 * <p>
 *     {@link Gate} that will lock all the gates in sequence.
 * </p>
 */
public final class Sequence implements Gate
{
    private final Gate delegate;

    private Sequence (Iterable<? extends Gate> parts)
    {
        Gate delegate = new Null();
        for (Gate part : parts)
        {
            delegate = new Pair(delegate, part);
        }
        this.delegate = delegate;
    }

    public static Sequence of (Gate... parts)
    {
        return new Sequence(Arrays.asList(parts));
    }

    public static Sequence of (Collection<? extends Gate> parts)
    {
        return new Sequence(parts);
    }

    @Override
    public boolean run (Runnable action)
    {
        return delegate.run(action);
    }
}
