package com.spacetimecat.concurrent.batch.duration;

public final class Millisecond extends Duration
{
    private final long amount;

    /**
     * @param amount
     * cannot be negative
     */
    public Millisecond (long amount)
    {
        if (amount < 0L) { throw new IllegalArgumentException("amount < 0L"); }
        this.amount = amount;
    }

    @Override
    public long inMillisecond ()
    {
        return amount;
    }

    @Override
    public String toString ()
    {
        return String.format("%s millisecond", amount);
    }
}
