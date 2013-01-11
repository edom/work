package id.web.edom.rmi;

import id.web.edom.io.Hub;
import id.web.edom.rmi.io.FrameReader;

import java.io.EOFException;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.ReadableByteChannel;
import java.nio.channels.SelectableChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.SocketChannel;
import java.nio.channels.WritableByteChannel;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.RejectedExecutionException;

import org.slf4j.Logger;

/**
 * Handles frame transput (input and output).
 *
 * @author erik
 */
public class Transput
{

    private static final Logger lt = Log.getTransput();
    private static final Logger lq = Log.getQueue();

    public static class Listener
    {
        public void receive (Frame f) throws Exception {}
        public void terminate () {}
    }

    private final Object lock = new Object();
    private final BlockingQueue<Frame> outputQueue;
    private final FrameReader fin;
    private final Hub hub;
    private final SelectableChannel in;
    private final SelectableChannel out;
    private final Transput.Listener listener;
    private final SelectionKey skin;
    private final SelectionKey skout;

    private final Hub.Listener tl = new Hub.Listener()
    {
        @Override
        public void terminate (SelectableChannel channel) throws Exception
        {
            super.terminate(channel);
            if ((!in.isOpen()) && (!out.isOpen())) { listener.terminate(); }
        }
        @Override
        public int read (ReadableByteChannel in) throws Exception
        {
            final Frame f;
            synchronized (lock)
            {
                f = fin.read();
                if (f == null) return 0;
            }
            listener.receive(f);
            return f.limit();
        }
        @Override
        public int write (WritableByteChannel out) throws Exception
        {
            return Transput.this.write(out);
        }
    };

    protected int read (Frame buffer) throws EOFException, IOException
    {
        final ReadableByteChannel _in = (ReadableByteChannel) in;
        final FrameImpl _buffer = (FrameImpl) buffer;
        final int n = _in.read(_buffer.getBuffer());
        if (n == -1) { throw new EOFException(); }
        if (lt.isTraceEnabled()) { lt.trace("Read {} bytes.", n); }
        return n;
    }

    protected int write (WritableByteChannel out) throws IOException
    {
        final FrameImpl f = (FrameImpl) outputQueue.peek();
        if (f == null)
        {
            synchronized (lock)
            {
                // Need to synchronize before checking for emptiness
                // since someone might have queued something in the meantime.
                if (outputQueue.isEmpty())
                {
                    if (lq.isDebugEnabled())
                    {
                        lq.debug("Output queue is empty.");
                    }
                    hub.setInterestOps(skout, skout.interestOps() & ~SelectionKey.OP_WRITE);
                }
            }
            return 0;
        }
        if (f.remaining() == 0)
        {
            synchronized (lock)
            {
                outputQueue.remove();
                if (lq.isTraceEnabled())
                {
                    lq.trace("size->{} out: {}.", outputQueue.size(), f);
                }
                lock.notifyAll();
            }
            return 0;
        }
        final ByteBuffer buffer = f.getBuffer();
        final int n = out.write(buffer);
        if (lt.isTraceEnabled())
        {
            lt.trace("Wrote {} bytes.", n);
        }
        return n;
    }

    private class FrameReaderSource implements FrameReader.Source
    {
        @Override
        public void read (Frame buffer) throws EOFException, IOException
        {
            Transput.this.read(buffer);
        }
    }

    protected Transput (int capacity, final SelectableChannel in, final SelectableChannel out, Frame buffer, Hub hub, final Transput.Listener listener) throws Exception
    {
        final FrameReader.Source source = new FrameReaderSource();
        this.in = in;
        this.out = out;
        this.outputQueue = new ArrayBlockingQueue<Frame>(capacity);
        this.fin = FrameReader.create(source, buffer);
        this.hub = hub;
        this.listener = listener;
        skin = hub.register(in, SelectionKey.OP_READ, tl);
        skout = (in == out) ? skin : hub.register(out, 0, tl);
    }

    /**
     *
     * @param capacity maximum number frames in output queue;
     * a large value can slow down shutdown when the queue is full
     * @param in input channel
     * @param out output channel; can be the same object as the input channel (which is the case when using a {@link SocketChannel})
     * @param buffer will contain the most recently received frame
     * @param listener will be called whenever a frame is received
     * @param hub
     * @throws IOException
     */
    public static Transput create (int capacity, SelectableChannel in, SelectableChannel out, Frame buffer, Hub hub, Transput.Listener listener) throws Exception
    {
        return new Transput(capacity, in, out, buffer, hub, listener);
    }

    public void close () throws Exception
    {
        synchronized (lock)
        {
            hub.cancel(skin);
            hub.cancel(skout);
            lock.notifyAll();
        }
    }

    public void send (Frame f) throws InterruptedException, RejectedExecutionException
    {
        f.computeLength();
        f.position(0);
        synchronized (lock)
        {
            for (;;)
            {
                if (!skout.isValid()) { throw new RejectedExecutionException(); }
                if (outputQueue.offer(f))
                {
                    if (lq.isTraceEnabled())
                    {
                        lq.trace("size->{} in: {}.", outputQueue.size(), f);
                    }
                    hub.setInterestOps(skout, skout.interestOps() | SelectionKey.OP_WRITE);
                    return;
                }
                if (lq.isDebugEnabled())
                {
                    lq.debug("Output queue is full.");
                }
                lock.wait();
            }
        }
    }
}
