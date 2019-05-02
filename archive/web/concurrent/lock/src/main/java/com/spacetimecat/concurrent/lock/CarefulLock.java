package com.spacetimecat.concurrent.lock;

import com.spacetimecat.java.lang.unexceptional.Risky;
import com.spacetimecat.java.lang.unexceptional.RiskyRunnable;
import com.spacetimecat.java.lang.unexceptional.RiskySupplier;
import com.spacetimecat.java.lang.unit.Unit;

public final class CarefulLock implements RiskyLock
{
    private final Lock delegate;

    public CarefulLock (Lock delegate)
    {
        this.delegate = delegate;
    }

    @Override
    public Risky<Boolean> acquire ()
    {
        return new RiskySupplier<>(delegate::acquire).get();
    }

    @Override
    public Risky<Unit> release ()
    {
        return new RiskyRunnable(delegate::release).get();
    }
}
