package com.spacetimecat.objmap;

import com.spacetimecat.objmap.annotation.Field;

import java.lang.reflect.Constructor;
import java.lang.reflect.Executable;
import java.lang.reflect.Parameter;

final class ExtClass<T>
{
    private final Class<T> inner;

    ExtClass (Class<T> inner)
    {
        this.inner = inner;
    }

    Constructor<?> findSuitableForRowMap ()
    {
        final String className = inner.getName();
        for (final Constructor<?> c : inner.getConstructors())
        {
            if (ext_executableHasParameterHavingColumnAnnotation(c))
            {
                return c;
            }
        }
        throw new IllegalArgumentException(className + "does not have any public constructors with Field-annotated parameters");
    }

    private static boolean ext_executableHasParameterHavingColumnAnnotation (Executable e)
    {
        for (final Parameter p : e.getParameters())
        {
            if (p.isAnnotationPresent(Field.class))
            {
                return true;
            }
        }
        return false;
    }
}
