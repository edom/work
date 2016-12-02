package com.spacetimecat.relational.jdbc2;

import com.spacetimecat.relational.dyno.Map2;
import com.spacetimecat.relational.dyno.MapInvocationHandler;
import com.spacetimecat.relational.dyno.StandardMap;
import com.spacetimecat.relational.jdbc2.sync.JdbcMethod;
import com.spacetimecat.relational.jdbc2.sync.TableDescribingInterface;

import java.lang.reflect.Proxy;
import java.sql.ResultSet;
import java.util.HashMap;
import java.util.Map;

final class Model<A>
{
    private final Class<A> cls;

    private Model (Class<A> cls)
    {
        this.cls = cls;
    }

    @SuppressWarnings("unchecked")
    public A read (Map2 data)
    {
        final ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
        final Class[] interfaces = {cls};
        final MapInvocationHandler handler = new MapInvocationHandler(data);
        return (A) Proxy.newProxyInstance(classLoader, interfaces, handler);
    }

    public A read (String prefix, ResultSet resultSet)
    {
        final Map<String, Object> map = new HashMap<>();
        final Map2 row = new ResultSetMap2(resultSet);
        for (final JdbcMethod method : new TableDescribingInterface(cls).getters())
        {
            final String propertyName = method.getPropertyName();
            final String columnName = prefix + method.getColumnName();
            final Object value = row.get(columnName);
            map.put(propertyName, value);
        }
        final Map2 data = new StandardMap(map);
        return read(data);
    }

    /**
     * <p>
     *     You cannot use this with interfaces that have type parameters,
     *     such as {@link java.util.Collection}.
     * </p>
     * @param interfaceClass
     * describe the interface
     * @param <A>
     * the interface type
     * @return
     * an instance
     */
    public static <A> Model<A> of (Class<A> interfaceClass)
    {
        if (!interfaceClass.isInterface()) { throw new IllegalArgumentException("!interfaceClass.isInterface()"); }
        return new Model<>(interfaceClass);
    }
}
