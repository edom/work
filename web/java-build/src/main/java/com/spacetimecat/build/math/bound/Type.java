package com.spacetimecat.build.math.bound;

import java.util.Comparator;

/**
 * <p>
 *     Open or closed.
 * </p>
 */
public final class Type
{
    private final boolean open;

    private Type (boolean open)
    {
        this.open = open;
    }

    public static Type closed ()
    {
        return new Type(false);
    }

    public static Type open ()
    {
        return new Type(true);
    }

    public boolean isOpen ()
    {
        return open;
    }

    @Override
    public String toString ()
    {
        return open ? "open" : "closed";
    }

    public static Comparator<Type> LowerComparator ()
    {
        return (a, b) ->
        {
            final int x = a.isOpen() ? 1 : 0;
            final int y = b.isOpen() ? 1 : 0;
            return x - y;
        };
    }

    public static Comparator<Type> UpperComparator ()
    {
        return (a, b) ->
        {
            final int x = a.isOpen() ? 1 : 0;
            final int y = b.isOpen() ? 1 : 0;
            return y - x;
        };
    }
}
