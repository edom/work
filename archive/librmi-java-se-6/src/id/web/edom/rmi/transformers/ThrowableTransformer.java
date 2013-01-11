package id.web.edom.rmi.transformers;

import id.web.edom.rmi.SerDes;
import id.web.edom.rmi.Transformer;
import id.web.edom.rmi.io.PrimitiveInput;
import id.web.edom.rmi.io.PrimitiveOutput;

import java.lang.reflect.Constructor;

public class ThrowableTransformer implements Transformer
{

    static
    {
        SerDes.getDefault().add(Throwable.class, new ThrowableTransformer());
    }

    private final StringTransformer s = new StringTransformer();

    @SuppressWarnings("rawtypes")
    public Class[] getTypes ()
    {
        return new Class[] { Throwable.class };
    }

    public Object read (PrimitiveInput r) throws Exception
    {
        final String className = (String) s.read(r);
        final String message = (String) s.read(r);
        final Class<?> c = Class.forName(className);
        final Constructor<?> co = c.getConstructor(String.class);
        return (Throwable) co.newInstance(message);
    }

    public Transformer write (Object o, PrimitiveOutput w) throws Exception
    {
        final Throwable t = (Throwable) o;
        s.write(t.getClass().getName(), w);
        s.write(t.getMessage(), w);
        return this;
    }

}
