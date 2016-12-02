package com.spacetimecat.build.math.vector;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

/**
 * <p>
 *     A vector of parts.
 * </p>
 *
 * @param <P> type of a part
 */
public final class Vector<P>
{
    private final List<P> parts;

    private Vector (List<P> parts)
    {
        this.parts = parts;
    }

    @SuppressWarnings("unchecked")
    public static <P> Vector<P> create (P... parts)
    {
        return new Vector<>(Arrays.asList(parts));
    }

    public static <P> Vector<P> create (List<P> parts)
    {
        return new Vector<>(parts);
    }

    public boolean isEmpty ()
    {
        return parts.isEmpty();
    }

    public int size ()
    {
        return parts.size();
    }

    public P get (int index)
    {
        return parts.get(index);
    }

    @Override
    public String toString ()
    {
        final List<String> parts = this.parts.stream().map(Object::toString).collect(Collectors.toList());
        return String.join("", parts);
    }
}
