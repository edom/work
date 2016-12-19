package com.spacetimecat.concurrent.batch.example;

import java.time.LocalDate;

public final class Request
{
    public final long thingId;
    public final LocalDate date;

    private final String string;

    public Request (long thingId, LocalDate date)
    {
        this.thingId = thingId;
        this.date = date;
        this.string = String.format("%-20s %-20s", thingId, date);
    }

    @Override
    public String toString ()
    {
        return string;
    }

    @Override
    public boolean equals (Object that)
    {
        if (this == that) { return true; }
        if (!(that instanceof Request)) { return false; }
        return this.string.equals(((Request) that).string);
    }

    @Override
    public int hashCode ()
    {
        return string.hashCode();
    }
}
