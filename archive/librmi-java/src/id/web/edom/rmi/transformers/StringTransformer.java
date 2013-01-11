package id.web.edom.rmi.transformers;

import id.web.edom.rmi.Transformer;
import id.web.edom.rmi.io.PrimitiveInput;
import id.web.edom.rmi.io.PrimitiveOutput;

import java.io.UnsupportedEncodingException;

public class StringTransformer implements Transformer
{

    private final PrimitiveByteArrayTransformer b = new PrimitiveByteArrayTransformer();
    private static final String CHARSET = "UTF-8";

    public Class[] getTypes ()
    {
        return new Class[] { String.class };
    }

    public Object read (PrimitiveInput r)
    {
        try { return new String((byte[]) b.read(r), CHARSET); }
        catch (UnsupportedEncodingException e) { throw new AssertionError(e); }
    }

    public Transformer write (Object o, PrimitiveOutput w)
    {
        try { b.write(((String) o).getBytes(CHARSET), w); }
        catch (UnsupportedEncodingException e) { throw new AssertionError(e); }
        return this;
    }

}
