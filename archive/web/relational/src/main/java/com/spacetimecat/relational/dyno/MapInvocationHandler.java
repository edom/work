package com.spacetimecat.relational.dyno;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;

public final class MapInvocationHandler implements InvocationHandler
{
    private final Map2 delegate;

    public MapInvocationHandler (Map2 delegate)
    {
        this.delegate = delegate;
    }

    @Override
    public Object invoke (Object proxy, Method method, Object[] args) throws Throwable
    {
        if (!method.getName().startsWith("get")) { return null; }
        final String propertyName = method.getName().substring(3);
        final Object value = delegate.get(propertyName);
        final Class<?> returnType = method.getReturnType();
        if (returnType.isPrimitive() && value == null)
        {
            // If we don't handle this,
            // the Java runtime will throw a NullPointerException
            // with confusing Proxy stack trace.
            final String message = String.format("Consider boxing the return type of %s", method);
            throw new NullPointerException(message);
        }
        return value;
    }
}
