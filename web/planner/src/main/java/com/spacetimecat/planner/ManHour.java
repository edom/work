package com.spacetimecat.planner;

public final class ManHour
{
    private final int value;

    public ManHour (int value)
    {
        this.value = value;
    }

    public int unwrap ()
    {
        return value;
    }

    @Override
    public String toString ()
    {
        return String.format("%d man-hour", value);
    }
}
