package id.web.edom.rmi.test;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import id.web.edom.net.Acceptor;
import id.web.edom.rmi.Endpoint;
import id.web.edom.rmi.EndpointBuilder;
import id.web.edom.rmi.Frame;
import id.web.edom.rmi.FrameFactory;
import id.web.edom.rmi.Java6Endpoint;
import id.web.edom.rmi.SerDes;
import id.web.edom.rmi.SpecialMethods;
import id.web.edom.rmi.Status;
import id.web.edom.rmi.test.back.Echo;
import id.web.edom.rmi.test.back.EchoImpl;
import id.web.edom.rmi.test.back.FakeEndpoint;
import id.web.edom.rmi.test.back.LoopbackTestWithTwoEndpoints;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.nio.channels.Channels;
import java.nio.channels.ReadableByteChannel;
import java.nio.channels.SelectableChannel;
import java.nio.channels.SocketChannel;
import java.nio.channels.WritableByteChannel;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.Ignore;
import org.junit.Test;

/**
 * This test case must succeed at all costs.
 * @author erik
 */
public class CorrectnessTest extends LoopbackTestWithTwoEndpoints
{
    private static final String CHARSET = "UTF-8";
    private final byte[] message;
    private final byte[] request;
    private final byte[] response;
    {
        try
        {
            final String m = "hello world";
            final Frame f = new FrameFactory().createFrame();
            final SerDes s = SerDes.getDefault();
            message = m.getBytes(CHARSET);
            {
                final ByteArrayOutputStream b = new ByteArrayOutputStream();

                f.reset();
                f.setSequenceNumber(0);
                f.setMethodNumber(SpecialMethods.LOOKUP);
                f.seekArguments();
                s.write(String.class, "echo", f);
                f.computeLength();
                b.write(f.toByteArray());

                f.reset();
                f.setSequenceNumber(0);
                f.setMethodNumber(0);
                f.seekArguments();
                s.write(String.class, m, f);
                f.computeLength();
                b.write(f.toByteArray());

                request = b.toByteArray();
            }
            {
                final ByteArrayOutputStream b = new ByteArrayOutputStream();

                f.reset();
                f.setSequenceNumber(0);
                f.setStatusNumber(Status.OK);
                f.seekArguments();
                s.write(int.class, 0, f);
                f.computeLength();
                b.write(f.toByteArray());

                f.reset();
                f.setSequenceNumber(0);
                f.setStatusNumber(Status.OK);
                f.seekArguments();
                s.write(String.class, m, f);
                f.computeLength();
                b.write(f.toByteArray());

                response = b.toByteArray();
            }
        }
        catch (Exception e) { throw new AssertionError(e); }
    }

    @Test
    public void acceptor () throws Exception
    {
        final AtomicInteger accepted = new AtomicInteger();
        final Acceptor a;
        final int n = 5;
        synchronized (accepted)
        {
            a = Acceptor.create(null, hub, new Acceptor.Listener()
            {
                @Override
                public void accept (SocketChannel c) throws Exception
                {
                    synchronized (accepted)
                    {
                        accepted.incrementAndGet();
                        accepted.notifyAll();
                    }
                    c.close();
                }
            });
            for (int i = 0; i < n; ++i)
            {
                final SocketChannel c = SocketChannel.open(a.getLocalSocketAddress());
                accepted.wait(1000);
                c.close();
            }
        }
        a.close();
        a.join();
        assertEquals(n, accepted.get());
    }

    @Ignore
    @Test
    public void testDeadInterlocutor () throws Throwable
    {
        final ClientServerTest t = new ClientServerTest()
        {
            @Override
            public Endpoint createServer (SelectableChannel in, SelectableChannel out) throws Exception
            {
                return new EndpointBuilder().setTimeout(250).setInput(in).setOutput(out).createEndpoint();
            }
            @Override
            public Endpoint createClient (SelectableChannel in, SelectableChannel out) throws Exception
            {
                final FakeEndpoint f = new FakeEndpoint(in, out)
                {
                    @Override
                    public void handle (Frame f) throws Exception
                    {
//                        throw new AssertionError();
                        System.out.println(f.toString());
                    }
                };
                f.start();
                return f;
            }
        };
        final NioFrame f = t.createFrame();
        final Endpoint s = t.getServer();
        final FakeEndpoint c = (FakeEndpoint) t.getClient();
        s.remoteInvoke(0, 0, void.class, new Object[0], new Class[0]);
        Thread.sleep(1000);
        t.shutdown();
        t.join();
    }

    @Test
    @Ignore
    public void testReceivingSide () throws Exception
    {
        final Object back = new EchoImpl();
        final ByteArrayInputStream bais = new ByteArrayInputStream(request);
        final ByteArrayOutputStream b = new ByteArrayOutputStream();
        final ReadableByteChannel in = Channels.newChannel(bais);
        final WritableByteChannel out = Channels.newChannel(b);
        final Endpoint s = new EndpointBuilder().export(back).setInput(in).setOutput(out).createEndpoint();
        s.join();
        assertArrayEquals(response, b.toByteArray());
    }

    @Test
    @Ignore
    public void testSendingSide () throws Exception
    {
        final ByteArrayOutputStream b = new ByteArrayOutputStream();
        final WritableByteChannel out = Channels.newChannel(b);
        final ReadableByteChannel in = Channels.newChannel(new ByteArrayInputStream(response));
        final Java6Endpoint e = new EndpointBuilder().setInput(in).setOutput(out).createEndpoint();
        final Echo a = (Echo) e.wrapRemote(0, new Class<?>[]{Echo.class});
        final byte[] c = a.echo(message);
        e.join();
        assertArrayEquals(message, c);
        assertArrayEquals(request, b.toByteArray());
    }

    @Test
    @Ignore
    public void both () throws Exception
    {
        final Echo a = (Echo) c.wrapRemote(0, new Class[]{Echo.class});
        final byte[] d = a.echo(message);
        c.shutdown();
        c.join();
        s.shutdown();
        s.join();
        assertArrayEquals(message, d);
    }

}
