package id.web.edom.rmi.transformers;

import id.web.edom.rmi.MethodDescription;
import id.web.edom.rmi.Transformer;
import id.web.edom.rmi.io.PrimitiveInput;
import id.web.edom.rmi.io.PrimitiveOutput;

public class MethodDescriptionTransformer implements Transformer
{
    private final StringTransformer s = new StringTransformer();
    private final ClassTransformer c = new ClassTransformer();

    public Class[] getTypes ()
    {
        return new Class[] { MethodDescription.class };
    }

    public Object read (PrimitiveInput r) throws Exception
    {
        final String name = (String) s.read(r);
        final Class returnType = (Class) c.read(r);
        final int np = r.readInt();
        final Class[] parameterTypes = new Class[np];
        for (int i = 0; i < np; ++i) { parameterTypes[i] = (Class) c.read(r); }
        return new MethodDescription(returnType, name, parameterTypes);
    }

    public Transformer write (Object o, PrimitiveOutput w) throws Exception
    {
        final MethodDescription m = (MethodDescription) o;
        final Class[] ps = m.getParameterTypes();
        s.write(m.getName(), w);
        c.write(m.getReturnType(), w);
        w.writeInt(ps.length);
        for (int i = 0; i < ps.length; ++i) { c.write(ps[i], w); }
        return this;
    }

}
