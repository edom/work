package id.web.edom.rmi.test;

import static org.junit.Assert.fail;
import id.web.edom.rmi.Frame;
import id.web.edom.rmi.FrameFactory;
import id.web.edom.rmi.Transput;
import id.web.edom.rmi.server.CountedTransput;
import id.web.edom.rmi.test.back.LoopbackTest;

import java.io.IOException;
import java.util.concurrent.RejectedExecutionException;

import org.junit.Test;

public class TransputTest extends LoopbackTest
{
    private boolean received;
    private final Object lock = new Object();
    protected final FrameFactory ff = new FrameFactory();
    protected CountedTransput t0;
    protected CountedTransput t1;
    
    @Override
    public void before () throws Exception
    {
        super.before();
        received = false;
        t0 = CountedTransput.create(8, p0.source(), p1.sink(), ff.createFrame(), new Transput.Listener());
        t1 = CountedTransput.create(8, p1.source(), p0.sink(), ff.createFrame(), new Transput.Listener()
        {
            @Override
            public void receive (Frame f) throws Exception
            {
                synchronized (lock)
                {
                    received = true;
                    lock.notifyAll();
                }
            }
        });
        t0.start();
        t1.start();
    }

    @Override
    public void after () throws Exception
    {
        t0.close();
        t1.close();
        t0.join();
        t1.join();
        super.after();
    }

    @Test
    public void transput () throws Exception
    {
        t0.send(ff.createFrame().setSequenceNumber(5).setObjectNumber(1).setMethodNumber(14).seekArguments());
        synchronized (lock)
        {
            if (!received) { lock.wait(250); }
            if (!received) { fail("out of luck"); }
        }
    }
    
    @Test(expected=RejectedExecutionException.class)
    public void reject () throws Exception
    {
        t0.close();
        t0.send(ff.createFrame());
    }
    
    /**
     * Sometimes prints an {@link IOException} with "Broken pipe" message,
     * sometimes not, depending on which thread terminates first.
     * 
     * @throws Exception
     */
    @Test
    public void shutdownWhileSending () throws Exception
    {
        final int cap = ff.getMaxFrameSize();
        for (int i = 0; i < 8; ++i)
        {
            t0.send(ff.createFrame().position(cap));
            t1.send(ff.createFrame().position(cap));
        }
        t0.close();
        t1.close();
        t0.join();
        t1.join();
    }
    
    @Test
    public void performance () throws Exception
    {
        final long n0 = System.nanoTime();
        final int n = 8192;
        final int cap = ff.getMaxFrameSize();
        for (int i = 0; i < n; ++i)
        {
            final Frame f = ff.createFrame();
            f.reset();
            f.position(cap);
            t1.send(f);
        }
        final long n1 = System.nanoTime();
        final long dn = (long) n * cap;
        final double dt = (n1 - n0) / 1.0e9;
        System.out.format("%d B / %f s = %f B/s = %f MiB/s\n", dn, dt, dn / dt, dn / dt / 1048576);
    }
    
}
