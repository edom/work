package id.web.edom.rmi.transformers;

import id.web.edom.rmi.Transformer;
import id.web.edom.rmi.io.PrimitiveInput;
import id.web.edom.rmi.io.PrimitiveOutput;

public class ClassTransformer implements Transformer
{
    private final StringTransformer s = new StringTransformer();
    public Class[] getTypes ()
    {
        return new Class[] { Class.class };
    }

    public Object read (PrimitiveInput r) throws ClassNotFoundException
    {
        final String name = (String) s.read(r);
        return Class.forName(name);
    }

    public Transformer write (Object o, PrimitiveOutput w)
    {
        final String name = ((Class) o).getName();
        s.write(name, w);
        return this;
    }

}
