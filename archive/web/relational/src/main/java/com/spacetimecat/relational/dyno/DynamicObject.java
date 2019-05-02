package com.spacetimecat.relational.dyno;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Proxy;

/**
 * <p>
 *     Forward {@link Map2#get(String)} to getters.
 * </p>
 */
public final class DynamicObject implements Map2
{
    private final Object delegate;

    public DynamicObject (Object delegate)
    {
        this.delegate = delegate;
    }

    @Override
    public Object get (String key)
    {
        return Property.named(key).getFrom(delegate);
    }

    @SuppressWarnings("unchecked")
    public <T> T as (Class<T> type)
    {
        if (!type.isInterface()) { throw new IllegalArgumentException("!type.isInterface()"); }
        final ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
        final Class[] interfaces = {type};
        final InvocationHandler handler = new MapInvocationHandler(this);
        return (T) Proxy.newProxyInstance(classLoader, interfaces, handler);
    }
}
