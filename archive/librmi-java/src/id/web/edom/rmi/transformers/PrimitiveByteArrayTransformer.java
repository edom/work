package id.web.edom.rmi.transformers;

import id.web.edom.rmi.Transformer;
import id.web.edom.rmi.io.PrimitiveInput;
import id.web.edom.rmi.io.PrimitiveOutput;

public class PrimitiveByteArrayTransformer implements Transformer
{

    public Class[] getTypes ()
    {
        return new Class[] { byte[].class };
    }

    public Object read (PrimitiveInput r)
    {
        final int length = r.readInt();
        final byte[] out = new byte[length];
        r.readFully(out);
        return out;
    }

    public Transformer write (Object o, PrimitiveOutput w)
    {
        final byte[] b = (byte[]) o;
        w.writeInt(b.length);
        w.write(b);
        return this;
    }

}
