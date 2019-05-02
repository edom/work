package com.spacetimecat.objmap;

import com.spacetimecat.UncheckedException;
import com.spacetimecat.collection.BasicMutableFailMap;
import com.spacetimecat.collection.MutableMap;
import com.spacetimecat.function.Tuple2;

import java.lang.reflect.Field;
import java.util.Map;
import java.util.NoSuchElementException;

/**
 * <p>Present an object as a {@link MutableMap}.</p>
 *
 * <p>To obtain an instance, use {@link #of(Object)}.</p>
 *
 * @param <A> type of underlying object
 *
 * @see ExtractDtoFields
 */
public final class SimpleObjectMap<A> implements MutableMap<String, Object>
{
    private final Class<?> cls;
    private final A object;
    private final Map<String, Field> fieldMap;

    private SimpleObjectMap (Class<?> cls, A object, Map<String, Field> fieldMap)
    {
        this.cls = cls;
        this.object = object;
        this.fieldMap = fieldMap;
    }

    public static <A> SimpleObjectMap<A> of (A object)
    {
        final Class<?> cls = object.getClass();
        final Map<String, Field> fieldMap = ExtractDtoFields.of(cls)
            .mapToBasicDumpableMap(field -> new Tuple2<>(field.getName(), field))
            .toNewStdMap();
        return new SimpleObjectMap<>(cls, object, fieldMap);
    }

    /**
     * <p>Return null if the field is not a public non-static non-final field.</p>
     * <p>If you need to tell null fields from nonexistent fields,
     * use {@link #getOrFail(String)}.</p>
     * @param fieldName name of a field of {@code A}
     * @return null if the field is not a public of {@code A} or if the field is null;
     *
     */
    @Override
    public Object get (String fieldName)
    {
        final Field field = fieldMap.get(fieldName);
        if (field == null) { return null; }
        return get(field);
    }

    /**
     * <p>Nonexistent fields are silently ignored.</p>
     * @param fieldName name of the field to mutate
     * @param newValue value to be put into the field
     * @return {@code this}
     */
    @Override
    public MutableMap<String, Object> put (String fieldName, Object newValue)
    {
        final Field field = fieldMap.get(fieldName);
        if (field == null) { return this; }
        return set(field, newValue);
    }

    @Override
    public Object getOrFail (String fieldName) throws NoSuchElementException
    {
        final Field field = findField(fieldName);
        return get(field);
    }

    @Override
    public BasicMutableFailMap<String, Object> putOrFail (String fieldName, Object newValue) throws NoSuchElementException
    {
        final Field field = findField(fieldName);
        return set(field, newValue);
    }

    private Field findField (String name)
    {
        final Field field = fieldMap.get(name);
        if (field == null) { throw new NoSuchElementException(cls.getName() + ": " + name); }
        return field;
    }

    private Object get (Field field)
    {
        try
        {
            return field.get(object);
        }
        catch (IllegalAccessException e)
        {
            throw new UncheckedException(e);
        }
    }

    private SimpleObjectMap set (Field field, Object newValue)
    {
        try
        {
            field.set(object, newValue);
            return this;
        }
        catch (IllegalAccessException e)
        {
            throw new UncheckedException(e);
        }
    }
}
