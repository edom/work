package id.web.edom.rmi.transformers;

import id.web.edom.rmi.Transformer;
import id.web.edom.rmi.io.PrimitiveInput;
import id.web.edom.rmi.io.PrimitiveOutput;

public class IntTransformer implements Transformer
{

    public Class[] getTypes ()
    {
        return new Class[] { int.class, Integer.class };
    }

    public Object read (PrimitiveInput r)
    {
        return new Integer(r.readInt());
    }

    public Transformer write (Object o, PrimitiveOutput w)
    {
        w.writeInt(((Integer) o).intValue());
        return this;
    }

}
