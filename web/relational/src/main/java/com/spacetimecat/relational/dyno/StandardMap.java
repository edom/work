package com.spacetimecat.relational.dyno;

import java.util.Map;

/**
 * <p>
 *     This forwards methods to the {@link Map} in Java standard library.
 * </p>
 */
public final class StandardMap implements Map2
{
    private final Map<String, Object> delegate;

    public StandardMap (Map<String, Object> delegate)
    {
        this.delegate = delegate;
    }

    @Override
    public Object get (String key)
    {
        return delegate.get(key);
    }
}
