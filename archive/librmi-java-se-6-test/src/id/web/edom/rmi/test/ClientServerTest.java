package id.web.edom.rmi.test;

import id.web.edom.rmi.Endpoint;
import id.web.edom.rmi.Frame;
import id.web.edom.rmi.FrameFactory;

import java.nio.channels.Pipe;
import java.nio.channels.SelectableChannel;

public abstract class ClientServerTest
{
    private final Pipe cs;
    private final Pipe sc;
    private final Endpoint c;
    private final Endpoint s;
    private final FrameFactory ff;

    public ClientServerTest () throws Exception
    {
        cs = Pipe.open();
        sc = Pipe.open();
        s = createServer(cs.source(), sc.sink());
        c = createClient(sc.source(), cs.sink());
        ff = new FrameFactory();
    }

    public Frame createFrame () { return ff.createFrame(); }

    public final Endpoint getClient () { return c; }
    public final Endpoint getServer () { return s; }

    public void shutdown () throws Exception
    {
        c.shutdown();
        s.shutdown();
    }

    public void join () throws Exception
    {
        c.join();
        s.join();
    }

    public abstract Endpoint createServer (SelectableChannel in, SelectableChannel out) throws Exception;
    public abstract Endpoint createClient (SelectableChannel in, SelectableChannel out) throws Exception;
}
