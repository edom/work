package com.spacetimecat.objmap;

import com.spacetimecat.collection.BasicMap;
import com.spacetimecat.collection.Iterable;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;

/**
 * <p>Create an object and sets its public fields based on entries in map.</p>
 *
 * <p>See {@link #unpack(BasicMap)} for more details.</p>
 *
 * @param <J> unpacked representation type
 */
final class BasicUnpackPublicFieldFromMap<J> implements BasicUnpack<BasicMap<String, Object>, J>
{
    private final Class<J> cls;
    private final Iterable<Field> fields;

    private BasicUnpackPublicFieldFromMap (Class<J> cls, Iterable<Field> fields)
    {
        this.cls = cls;
        this.fields = fields;
    }

    static <A> BasicUnpackPublicFieldFromMap<A> of (Class<A> cls)
    {
        final int cm = cls.getModifiers();
        final String className = cls.getName();
        if (Modifier.isInterface(cm))
        {
            throw new IllegalArgumentException("need public concrete class, given interface: " + className);
        }
        if (Modifier.isAbstract(cm))
        {
            throw new IllegalArgumentException("need public concrete class, given abstract class: " + className);
        }
        if (!Modifier.isPublic(cm))
        {
            throw new IllegalArgumentException("please make the class public: " + className);
        }
        try { cls.getConstructor(); }
        catch (NoSuchMethodException e)
        {
            throw new IllegalArgumentException("please make a public no-argument constructor in " + className, e);
        }
        return new BasicUnpackPublicFieldFromMap<>(cls, ExtractDtoFields.of(cls));
    }

    /**
     * <p>Create an instance of J and set its public fields
     * according to the given map.</p>
     *
     * <p>Let R be the returned object.</p>
     *
     * <p>Let M be the map argument.</p>
     *
     * <p>For each public non-static non-final field F in J,
     * this sets R.F to {@code V = M.get(F.getName())} if V is not null.
     * However, some maps throw an exception instead of returning null
     * if a key is not found.</p>
     *
     * <p>The type J must be a concrete class
     * that has a no-argument constructor.</p>
     *
     * <p>Instances of J should be treated as Data Transfer Objects
     * whose sole purpose is describing data format
     * such as JSON format or SQL table format,
     * and therefore should not have behaviors.</p>
     */
    @Override
    public J unpack (BasicMap<String, Object> map)
    {
        try
        {
            final J result = cls.newInstance();
            fields.forEach(field ->
            {
                assert ExtractDtoFields.isWritableInstanceField(field);
                final String k = field.getName();
                final Object v = map.get(k);
                if (v == null) { return; }
                try { field.set(result, v); }
                catch (IllegalAccessException e) { throw new ReadException(e); }
            });
            return result;
        }
        catch (IllegalAccessException e)
        {
            throw new ReadException(cls.getName()
                + ": no-argument constructor must be public"
                + ", and class must also be publicly accessible"
                , e);
        }
        catch (InstantiationException e)
        {
            throw new ReadException(cls.getName()
                + ": instance creation failed"
                , e);
        }
        catch (ExceptionInInitializerError e)
        {
            throw new ReadException(cls.getName()
                + ": instance initialization failed"
                , e.getCause());
        }
    }
}
