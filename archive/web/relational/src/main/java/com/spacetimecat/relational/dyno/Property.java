package com.spacetimecat.relational.dyno;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

/**
 * <p>
 *     Property that slightly deviates from Java bean conventions.
 * </p>
 */
final class Property
{
    private final String name;

    private Property (String name)
    {
        if (name == null) { throw new NullPointerException("name"); }
        if (name.isEmpty()) { throw new IllegalArgumentException("name.isEmpty()"); }
        this.name = name;
    }

    public static Property named (String name)
    {
        return new Property(name);
    }

    public Object getFrom (Object instance)
    {
        try
        {
            final Method method = getterFor(instance);
            return method.invoke(instance);
        }
        catch (IllegalAccessException e)
        {
            throw new ProgrammingException(e);
        }
        catch (InvocationTargetException e)
        {
            final Throwable cause = e.getCause();
            if (cause instanceof RuntimeException) { throw (RuntimeException) cause; }
            if (cause instanceof Error) { throw (Error) cause; }
            throw new RuntimeException(cause);
        }
    }

    public String getterName ()
    {
        return "get" + name;
    }

    public Method getterFor (Object instance)
    {
        try
        {
            final String getterName = getterName();
            return instance.getClass().getMethod(getterName);
        }
        catch (NoSuchMethodException e)
        {
            throw new PropertyNotFoundException(e);
        }
    }
}
