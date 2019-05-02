package com.spacetimecat.build.dependency;

import com.spacetimecat.build.math.vector.Vector;

import java.util.Comparator;

public final class Version
{
    private final Vector<String> delegate;

    public Version (Vector<String> delegate)
    {
        this.delegate = delegate;
    }

    public boolean isEmpty ()
    {
        return delegate.isEmpty();
    }

    @Override
    public String toString ()
    {
        return delegate.toString();
    }

    public static Comparator<Version> Comparator (Comparator<Vector<String>> underlying)
    {
        return (a, b) -> underlying.compare(a.delegate, b.delegate);
    }
}
