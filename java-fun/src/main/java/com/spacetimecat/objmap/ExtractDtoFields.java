package com.spacetimecat.objmap;

import com.spacetimecat.collection.Iterable;
import com.spacetimecat.collection.Iterables;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;

/**
 * <p>Extract writable instance fields: public, non-final, non-static fields.</p>
 */
final class ExtractDtoFields
{
    private ExtractDtoFields () {}

    static Iterable<Field> of (Class<?> cls)
    {
        return
            Iterables.from(cls.getFields())
            .filter(ExtractDtoFields::isWritableInstanceField)
            .eager()
        ;
    }

    static boolean isWritableInstanceField (Field field)
    {
        final int m = field.getModifiers();
        return
            Modifier.isPublic(m)
                && !Modifier.isFinal(m)
                && !Modifier.isStatic(m);
    }
}
