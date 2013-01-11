package id.web.edom.rmi.test.back;

import id.web.edom.rmi.Endpoint;
import id.web.edom.rmi.Frame;
import id.web.edom.rmi.FrameFactory;
import id.web.edom.rmi.FrameHandler;
import id.web.edom.rmi.server.CountedTransput;

import java.io.IOException;
import java.nio.channels.SelectableChannel;

/**
 * Fake {@link Endpoint} for testing.
 *
 * @author erik
 */
public abstract class FakeEndpoint implements Endpoint, FrameHandler
{
    private final CountedTransput t;
    public FakeEndpoint (SelectableChannel in, SelectableChannel out) throws IOException
    {
        final FrameFactory ff = new FrameFactory();
        t = new CountedTransput(4, in, out, ff.createFrame(), this, new Runnable()
        {
            @Override
            public void run ()
            {
            }
        });
    }

    public final void start () { t.start(); }

    @Override
    public final Endpoint shutdown () throws Exception
    {
        t.close();
        return this;
    }
    @Override
    public final Endpoint join () throws Exception
    {
        t.join();
        return this;
    }

    /**
     * Not implemented.
     * @throws UnsupportedOperationException always
     */
    @Override
    public final Object remoteInvoke (int object, int method, Class returnType, Object[] args, Class[] argTypes)
    {
        throw new UnsupportedOperationException();
    }

    /**
     * Override this.
     */
    @Override
    public abstract void handle (Frame f) throws Exception;

    public final void send (Frame f) throws InterruptedException
    {
        t.send(f);
    }
}
