package id.web.edom.rmi.test.back;

import java.nio.channels.Pipe;
import java.nio.channels.SelectableChannel;

public class Loopback
{
    private final Pipe cs;
    private final Pipe sc;

    public Loopback () throws Exception
    {
        cs = Pipe.open();
        sc = Pipe.open();
    }

    public SelectableChannel getClientInput () { return sc.source(); }
    public SelectableChannel getClientOutput () { return cs.sink(); }
    public SelectableChannel getServerInput () { return cs.source(); }
    public SelectableChannel getServerOutput () { return sc.sink(); }
}
