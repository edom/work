package id.web.edom.rmi.server;

import id.web.edom.io.Hub;
import id.web.edom.rmi.Frame;
import id.web.edom.rmi.Transput;

import java.io.EOFException;
import java.io.IOException;
import java.nio.channels.SelectableChannel;
import java.nio.channels.WritableByteChannel;
import java.util.concurrent.atomic.AtomicLong;

public class CountedTransput extends Transput
{
    private final AtomicLong bytesRead = new AtomicLong();
    private final AtomicLong bytesWritten = new AtomicLong();

    protected CountedTransput (int capacity, SelectableChannel in, SelectableChannel out, Frame buffer, Hub hub, Transput.Listener listener) throws Exception
    {
        super(capacity, in, out, buffer, hub, listener);
    }

    public static CountedTransput create (int capacity, SelectableChannel in, SelectableChannel out, Frame buffer, Hub hub, Transput.Listener listener) throws Exception
    {
        return new CountedTransput(capacity, in, out, buffer, hub, listener);
    }

    @Override
    protected int read (Frame buffer) throws EOFException, IOException
    {
        final int n = super.read(buffer);
        bytesRead.addAndGet(n);
        return n;
    }

    @Override
    protected int write (WritableByteChannel out) throws IOException
    {
        final int n = super.write(out);
        bytesWritten.addAndGet(n);
        return n;
    }

    public long getBytesRead () { return bytesRead.get(); }
    public long getBytesWritten () { return bytesWritten.get(); }
}
