package com.spacetimecat.concurrent.lock;

import com.spacetimecat.java.lang.unexceptional.Risky;
import com.spacetimecat.java.lang.unit.Unit;

/**
 * <p>
 *     A {@link Lock} whose methods can fail for other reasons beside normal contention.
 * </p>
 */
public interface RiskyLock
{
    Risky<Boolean> acquire ();

    Risky<Unit> release ();
}
