package com.spacetimecat.concurrent.gate;

import java.util.Objects;

/**
 * <p>
 *     {@link Gate} that will lock both gates.
 * </p>
 */
public final class Pair implements Gate
{
    private final Gate first;
    private final Gate second;

    /**
     * <p>
     *     A mutex whose {@link #run(Runnable) run}
     *     acquires the first mutex,
     *     and then acquires the second mutex,
     *     and then run the action.
     * </p>
     * @param first
     * the first to acquire, the second to release; must differ from the second
     * @param second
     * the second to acquire, the first to release; must differ from the first
     */
    public Pair (Gate first, Gate second)
    {
        Objects.requireNonNull(first, "first");
        Objects.requireNonNull(second, "second");
        if (first == second) { throw new IllegalArgumentException("first == second"); }
        this.first = first;
        this.second = second;
    }

    @Override
    public boolean run (Runnable action)
    {
        return first.run(() -> second.run(action));
    }
}
