package id.web.edom.rmi;

import id.web.edom.io.Hub;

import java.nio.channels.Channel;
import java.nio.channels.SelectableChannel;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * <p>Creates an {@link Endpoint}.</p>
 *
 * <p>Instances of this class are not thread-safe.</p>
 *
 * @author erik
 */
public class EndpointBuilder
{
    private Channel in;
    private Channel out;
    private ExecutorService executor;
    private long timeout = 30000;
    private final List<Object> objects = new ArrayList<Object>();
    private FrameFactory ff;
    private Hub hub;
    private Endpoint.Listener listener;
    public EndpointBuilder () {}
    public EndpointBuilder setExecutorService (ExecutorService executor) { this.executor = executor; return this; }
    /**
     * The class of the object must be public.
     * @param o the object to export
     * @return this
     */
    public EndpointBuilder export (Object o) { objects.add(o); return this; }
    /**
     * Calls {@link #export(Object)} for each object.
     * @param o
     * @return this
     */
    public EndpointBuilder exportAll (Collection<? extends Object> o) { objects.addAll(o); return this; }
    public EndpointBuilder setInput (Channel c) { in = c; return this; }
    public EndpointBuilder setOutput (Channel c) { out = c; return this; }
    public EndpointBuilder setListener (Endpoint.Listener r) { listener = r; return this; }
    public EndpointBuilder setFrameFactory (FrameFactory f) { ff = f; return this; }
    /**
     * Sets the maximum time to wait for response, in milliseconds.
     * @param t timeout in milliseconds
     * @return this
     */
    public EndpointBuilder setTimeout (long t) { timeout = t; return this; }
    public EndpointBuilder setHub (Hub h) { hub = h; return this; }
    /**
     * Do not call this more than once.
     * @return an endpoint created according to the given specifications
     * @throws Exception
     */
    public Java6Endpoint createEndpoint () throws Exception
    {
        if (hub == null) { throw new IllegalStateException("hub cannot be null"); }
        if (listener == null) { listener = new Endpoint.Listener(); }
        if (executor == null) { executor = Executors.newSingleThreadExecutor(); }
        if (ff == null) { ff = new FrameFactory(); }
        final Object[] objects = this.objects.toArray();
        if ((in instanceof SelectableChannel) && (out instanceof SelectableChannel))
        {
            final SelectableChannel sin = (SelectableChannel) in;
            final SelectableChannel sout = (SelectableChannel) out;
            final Dispatch dispatcher = new DefaultDispatch(objects, executor);
            final DefaultEndpoint e = new DefaultEndpoint(dispatcher, ff, 2, timeout, sin, sout, listener, hub);
            e.start();
            return e;
        }
        throw new UnsupportedOperationException("only selectable channels are supported");
    }
}
