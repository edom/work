package id.web.edom.rmi.test;

import id.web.edom.io.Hub;
import id.web.edom.rmi.Frame;
import id.web.edom.rmi.FrameFactory;
import id.web.edom.rmi.test.back.Loopback;

import java.nio.channels.ReadableByteChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.WritableByteChannel;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Test;

public class HubPerformanceTest
{
    @Test
    public void test () throws Exception
    {
        final AtomicInteger nr = new AtomicInteger();
        final AtomicInteger nw = new AtomicInteger();
        final Hub m = Hub.create();
        final FrameFactory ff = new FrameFactory();
        ff.setMaxFrameSize(1024);
        final int numPairs = 32;
        final long t0 = System.nanoTime();
        for (int i = 0; i < numPairs; ++i)
        {
            final Loopback l = new Loopback();
            final Hub.Listener c = new Hub.Listener()
            {
                final Frame f = ff.createFrame();
                @Override
                public void read (ReadableByteChannel in) throws Exception
                {
                    f.reset();
                    f.readFrom(in);
                    nr.incrementAndGet();
                }
                @Override
                public void write (WritableByteChannel out) throws Exception
                {
                    f.reset();
                    f.writeTo(out);
                    nw.incrementAndGet();
                }
            };
            m.register(l.getClientInput(), SelectionKey.OP_READ, c);
            m.register(l.getClientOutput(), SelectionKey.OP_WRITE, c);
            m.register(l.getServerInput(), SelectionKey.OP_READ, c);
            m.register(l.getServerOutput(), SelectionKey.OP_WRITE, c);
        }
        final long t1 = System.nanoTime();
        Thread.sleep(3000);
        m.stop();
        m.join();
        final long t2 = System.nanoTime();
        final double dt01 = (t1 - t0) / 1.0e9;
        final double dt12 = (t2 - t1) / 1.0e9;
        final int nrw = nr.get() + nw.get();
        final int nc = 4 * numPairs;
        System.out.format(
                "%d registrations / %f seconds = %f registrations/second\n" +
        		"%d reads and %d writes in %f seconds = %f dispatches/second\n",
        		nc, dt01, nc / dt01,
        		nr.get(), nw.get(), dt12, nrw / dt12);
    }
}
