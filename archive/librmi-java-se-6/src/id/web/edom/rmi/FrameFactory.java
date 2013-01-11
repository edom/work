package id.web.edom.rmi;

import id.web.edom.rmi.Frame;

import java.util.concurrent.atomic.AtomicInteger;

/**
 * <p>Creates a {@link Frame}.</p>
 *
 * <p>Instances of this class are thread-safe.</p>
 *
 * @author erik
 */
public class FrameFactory
{
    private static final int DEFAULT_CAPACITY = 4096;
    private final AtomicInteger maxFrameSize = new AtomicInteger(DEFAULT_CAPACITY);
    public FrameFactory () {}
    public int getMaxFrameSize () { return maxFrameSize.get(); }
    public FrameFactory setMaxFrameSize (int m)
    {
        maxFrameSize.set(m);
        return this;
    }
    public Frame createFrame ()
    {
        return new FrameImpl(maxFrameSize.get());
    }
    public static int getDefaultCapacity () { return DEFAULT_CAPACITY; }
}
