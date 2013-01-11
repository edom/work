package id.web.edom.rmi;

import id.web.edom.io.Hub;

import java.lang.reflect.Proxy;
import java.nio.channels.SelectableChannel;

import org.slf4j.Logger;

class DefaultEndpoint implements Java6Endpoint
{
    private static final Logger ld = Log.getDispatch();
    private static final Logger lt = Log.getTransput();
    private final long timeout;
    private final Transput transput;
    private final FrameFactory ff;
    private final Dispatch dispatcher;
    private final FrameHandler[] runs;
    private final SerDes serdes = SerDes.getDefault();
    private final Listener listener;

    private class TransputListener extends Transput.Listener
    {
        @Override
        public void receive (Frame f) throws Exception
        {
            DefaultEndpoint.this.receive(f);
        }

        @Override
        public void terminate ()
        {
            listener.terminate(DefaultEndpoint.this);
        }
    }

    private void receive (Frame f) throws Exception
    {
        if (lt.isTraceEnabled())
        {
            lt.trace("Received {}.", f);
        }
        if (f.isCall()) handleCall(f);
        else handleReturn(f);
    }

    DefaultEndpoint (Dispatch dispatch, FrameFactory ff, int capacity, long timeout, SelectableChannel sin, SelectableChannel sout, Listener listener, Hub hub) throws Exception
    {
        this.transput = Transput.create(capacity, sin, sout, ff.createFrame(), hub, new TransputListener());
        this.dispatcher = dispatch;
        this.listener = listener;
        this.timeout = timeout;
        this.runs = new FrameHandler[capacity];
        this.ff = ff;
    }
    public void start () { }
    @Override
    public Endpoint shutdown () throws Exception
    {
        dispatcher.stop();
        transput.close();
        return this;
    }
    @Override
    public Endpoint join () throws Exception
    {
        dispatcher.join();
        return this;
    }
    @Override
    public Object wrapRemote (int object, Class<?>[] interfaces)
    {
        return Proxy.newProxyInstance(getClass().getClassLoader(), interfaces, new ProxyInvocationHandler(object, this));
    }

    private void handleCall (final Frame f)
    {
        final int s = f.getSequenceNumber();
        final int object = f.getObjectNumber();
        final int method = f.getMethodNumber();
        final Dispatch.Listener returner = new Dispatch.Listener()
        {
            private void doReturn (int status, Object value, @SuppressWarnings("rawtypes") Class type) throws Exception
            {
                final Frame g = ff.createFrame();
                g.setSequenceNumber(s);
                g.seekArguments();
                g.setStatusNumber(status);
                serdes.write(type, value, g);
                try { transput.send(g); }
                catch (InterruptedException e) {} // XXX?
            }
            @Override
            public void ok (Object value, @SuppressWarnings("rawtypes") Class type)
            {
                try { doReturn(Status.OK, value, type); }
                catch (Exception e) { error(e); }
            }
            @Override
            public void error (Throwable t)
            {
                try { doReturn(Status.ERROR, t, Throwable.class); }
                // Exception while handling an exception is fatal.
                catch (Exception e) { throw new Error(e); }
            }
        };
        try
        {
            final Object[] args;
            if (method == SpecialMethods.LOOKUP)
            {
                final String name = (String) serdes.read(String.class, f);
                args = new Object[]{name};
            }
            else args = deserializeArguments(f, dispatcher.getParameterTypes(object, method));
            dispatcher.dispatch(object, method, args, returner);
        }
        catch (Exception e)
        {
            ld.error("Exception while dispatching.", e);
            returner.error(e);
            return;
        }
    }

    private Object[] deserializeArguments (Frame f, Class<?>[] pts) throws Exception
    {
        final int n = pts.length;
        final Object[] args = new Object[n];
        for (int i = 0; i < n; ++i)
        {
            args[i] = serdes.read(pts[i], f);
        }
        return args;
    }

    // TODO rename to register
    private int register (FrameHandler r) throws InterruptedException
    {
        if (r == null) throw new IllegalArgumentException();
        int s;
        // Find an unused sequence number.
        // If there isn't any, wait until there is.
        synchronized (runs)
        {
            out: for (;;)
            {
                for (s = 0; s < runs.length; ++s)
                {
                    if (runs[s] == null) break out;
                }
                runs.wait();
            }
            runs[s] = r;
        }
        return s;
    }

    private void handleReturn (Frame f) throws Exception
    {
        final int s = f.getSequenceNumber();
        if (!((0 <= s) && (s < runs.length))) throw new InvalidSequenceNumberException(s);
        final FrameHandler r;
        synchronized (runs)
        {
            r = runs[s];
            if (r == null) throw new UnexpectedReturnFrameException(s);
            runs[s] = null;
            runs.notifyAll();
        }
        r.handle(f);
    }

    public Object remoteInvoke (int object, int method, @SuppressWarnings("rawtypes") final Class returnType, Object[] args, @SuppressWarnings("rawtypes") Class[] argTypes) throws Exception
    {
        final Frame f = ff.createFrame();
        f.setMethodNumber(method);
        f.setObjectNumber(object);
        f.seekArguments();
        for (int i = 0; i < args.length; ++i)
        {
            serdes.write(argTypes[i], args[i], f);
        }
        final DefaultReturnFrameHandler h = new DefaultReturnFrameHandler(returnType, timeout);
        final int s = register(h);
        f.setSequenceNumber(s);
        transput.send(f);
        return h.get();
    }
}
