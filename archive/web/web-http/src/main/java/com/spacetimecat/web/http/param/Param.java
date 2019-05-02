package com.spacetimecat.web.http.param;

import java.util.function.Consumer;
import java.util.function.Function;

/**
 * <p>
 *     A named thing that can be null.
 * </p>
 *
 * @param <T> value type
 */
public class Param<T>
{
    private final String name;
    private final T value;

    public Param (String name, T value)
    {
        this.name = name;
        this.value = value;
    }

    public final String name ()
    {
        return name;
    }

    public final boolean exists ()
    {
        return value != null;
    }

    public final T optional ()
    {
        return value;
    }

    /**
     * <p>
     *     If the value is not null, return it; otherwise throw something.
     * </p>
     * <p>
     *     Override {@link #missing()} if you want to throw a different kind of exception.
     * </p>
     * @return not null
     * @throws MissingParamException if the value is null
     */
    public final T required ()
    {
        if (value == null)
        {
            missing();
            throw new MissingParamException(name);
        }
        return value;
    }

    /**
     * <p>
     *     Override this if you want to throw a different kind of exception.
     * </p>
     * <p>
     *     This is called by {@link #required()} if the value is null.
     * </p>
     */
    protected void missing ()
    {
    }

    public final <A> A ifExistsApply (Function<T, A> function)
    {
        if (exists())
        {
            return function.apply(value);
        }
        return null;
    }

    public final void ifExistsDo (Consumer<T> action)
    {
        if (exists())
        {
            action.accept(value);
        }
    }

    @Override
    public String toString ()
    {
        return String.format("%s=%s", name, value);
    }
}
