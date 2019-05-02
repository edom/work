package com.spacetimecat.java.lang.unexceptional;

import com.spacetimecat.java.lang.unit.Unit;

import java.util.function.Supplier;

public final class RiskyRunnable implements Supplier<Risky<Unit>>
{
    private final Runnable delegate;

    public RiskyRunnable (Runnable delegate)
    {
        this.delegate = delegate;
    }

    @Override
    public Risky<Unit> get ()
    {
        try
        {
            delegate.run();
            return new Ok<>(Unit.instance);
        }
        catch (Throwable e)
        {
            return new Fail<>(e);
        }
    }
}
