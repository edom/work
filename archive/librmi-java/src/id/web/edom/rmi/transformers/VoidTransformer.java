package id.web.edom.rmi.transformers;

import id.web.edom.rmi.Transformer;
import id.web.edom.rmi.io.PrimitiveInput;
import id.web.edom.rmi.io.PrimitiveOutput;

public class VoidTransformer implements Transformer
{

    public Class[] getTypes ()
    {
        return new Class[] { void.class, Void.class };
    }

    public Object read (PrimitiveInput r)
    {
        return null;
    }

    public Transformer write (Object o, PrimitiveOutput w)
    {
        return this;
    }

}
