package id.web.edom.rmi.test;

import static org.junit.Assert.assertEquals;
import id.web.edom.io.Hub;
import id.web.edom.rmi.test.back.Loopback;

import java.nio.ByteBuffer;
import java.nio.channels.ReadableByteChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.WritableByteChannel;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Test;

/**
 * Common usage test.
 * 
 * @author erik
 */
public class CommonTest
{
    @Test
    public void selector () throws Exception
    {
        final Loopback l = new Loopback();
        final AtomicInteger nr = new AtomicInteger();
        final AtomicInteger nw = new AtomicInteger();
        final Hub.Listener g = new Hub.Listener()
        {
            @Override
            public void read (ReadableByteChannel in) throws Exception
            {
                final ByteBuffer b = ByteBuffer.allocate(1024);
                final int n = in.read(b);
                nr.addAndGet(n);
            }
            @Override
            public void write (WritableByteChannel out) throws Exception
            {
                if (nw.get() >= 1048576)
                {
                    synchronized (nw) { nw.notifyAll(); }
                    return;
                }
                final int n = 4096;
                final ByteBuffer b = ByteBuffer.allocate(n);
                final int m = out.write(b);
                nw.addAndGet(m);
            }
        };
        final Hub m = Hub.create();
        synchronized (nw)
        {
            m.register(l.getClientInput(), SelectionKey.OP_READ, g);
            m.register(l.getClientOutput(), SelectionKey.OP_WRITE, g);
            m.register(l.getServerInput(), SelectionKey.OP_READ, g);
            m.register(l.getServerOutput(), SelectionKey.OP_WRITE, g);
            nw.wait(1024); // XXX
        }
        Thread.sleep(128);
        m.stop();
        m.join();
        assertEquals("transfer took too long", nw.get(), nr.get());
    }
}
