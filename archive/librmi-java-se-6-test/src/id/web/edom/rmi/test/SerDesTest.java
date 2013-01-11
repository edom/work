package id.web.edom.rmi.test;

import static org.junit.Assert.assertEquals;
import id.web.edom.rmi.Frame;
import id.web.edom.rmi.FrameFactory;
import id.web.edom.rmi.MethodDescription;
import id.web.edom.rmi.SerDes;
import id.web.edom.rmi.io.PrimitiveInput;
import id.web.edom.rmi.io.PrimitiveOutput;

import org.junit.Test;

public class SerDesTest
{
    protected final FrameFactory ff = new FrameFactory();
    protected final SerDes p = SerDes.getDefault();
    protected Frame f = ff.createFrame();
    
    @Test
    public void methodDescription () throws Exception
    {
        test(MethodDescription.class, new MethodDescription(int.class, "test", new Class[]{int.class, Integer.class}));
    }
    
    @Test
    public void primitiveInteger () throws Exception
    {
        test(int.class, 12345);
    }

    @Test
    public void integer () throws Exception
    {
        test(Integer.class, 12345);
    }
    
    @Test
    public void nonPrimitiveClass () throws Exception
    {
        test(Class.class, Class.class);
    }

    @Test(expected=ClassNotFoundException.class)
    public void primitiveClass () throws Exception
    {
        test(Class.class, int.class);
    }
    
    @Test
    public void string () throws Exception
    {
        test(String.class, "hello world");
    }
    
    private void test (Class<?> c, Object in) throws Exception
    {
        final PrimitiveInput r = (PrimitiveInput) f;
        final PrimitiveOutput w = (PrimitiveOutput) f;
        f.reset();
        p.write(c, in, w);
        f.reset();
        final Object out = p.read(c, r);
        assertEquals(in, out);
    }
}
