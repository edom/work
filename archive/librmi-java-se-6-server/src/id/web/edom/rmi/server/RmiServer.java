package id.web.edom.rmi.server;

import id.web.edom.Threaded;
import id.web.edom.io.Hub;
import id.web.edom.net.Acceptor;
import id.web.edom.rmi.Endpoint;
import id.web.edom.rmi.EndpointBuilder;
import id.web.edom.rmi.EndpointBuilderFactory;
import id.web.edom.rmi.FrameFactory;
import id.web.edom.rmi.Log;

import java.io.IOException;
import java.lang.management.ManagementFactory;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicInteger;

import javax.management.MBeanServer;
import javax.management.ObjectName;

import org.slf4j.Logger;

/**
 * <p>Remote procedure call is implemented as
 * remote method invocation of exactly one object.
 * Spawns an endpoint for each accepted connection.</p>
 *
 * <p>You can terminate the server via JMX.</p>
 *
 * @author erik
 */
public class RmiServer implements Threaded
{
    private static final Logger log = Log.getServer();

    public static class Listener
    {
        public void terminate () throws Exception {}
    }

    private final EndpointBuilderFactory factory;
    /**
     * <p>The set of all active endpoints in this server.</p>
     */
    private final Set<Endpoint> endpoints;
    /**
     * <p>The maximum number of active endpoints allowed in this server.</p>
     */
    private final AtomicInteger maxConnections = new AtomicInteger();
    private final AtomicInteger maxFrameSize = new AtomicInteger(FrameFactory.getDefaultCapacity());
    private final SocketAddress address;
    private final Instrumentation bean;
    private final Listener listener;
    private final long startTime;
    private final Hub hub;

    private static final String JMX_DOMAIN = "id.web.edom";
    private static final String JMX_TYPE = RmiServer.class.getSimpleName();

    private final class AcceptorListener extends Acceptor.Listener
    {
        private final class EndpointListener extends Endpoint.Listener
        {
            @Override
            public void terminate (Endpoint e)
            {
                synchronized (endpoints) { endpoints.remove(e); }
            }
        }
        @Override
        public void accept (ServerSocketChannel s) throws Exception
        {
            final SocketChannel c = s.accept();
            Log.accept(c);
            synchronized (endpoints)
            {
                if (endpoints.size() >= maxConnections.get())
                {
                    throw new TooManyEndpointsException();
                }
                final Endpoint.Listener listener = new EndpointListener();
                final EndpointBuilder builder = factory.createEndpointBuilder();
                final FrameFactory f = new FrameFactory();
                f.setMaxFrameSize(maxFrameSize.get());
                builder.setHub(hub).setInput(c).setOutput(c).setListener(listener).setFrameFactory(f);
                final Endpoint e = builder.createEndpoint();
                endpoints.add(e);
            }
        }
        @Override
        public void terminate () throws Exception
        {
            try { synchronized (endpoints) { endpoints.clear(); } }
            finally { listener.terminate(); }
        }
    }

    private RmiServer (String name, SocketAddress address, EndpointBuilderFactory factory, int maxEndpoints, Listener listener) throws Exception
    {
        log.info("Starting server named \"" + name + "\" at " + address + ".");
        this.startTime = System.currentTimeMillis();
        this.listener = listener;
        this.bean = new Instrumentation();
        this.address = address;
        this.factory = factory;
        this.endpoints = new HashSet<Endpoint>(maxEndpoints);
        this.maxConnections.set(maxEndpoints);
        this.hub = Hub.create();
    }

    /**
     * @param name name for management via JMX
     * @param factory generates a backing object each time an endpoint is created
     * @param maxEndpoints limits the number of simultaneously active endpoints
     * @param address the address to bind to
     * @throws Exception
     */
    public static RmiServer create (String name, EndpointBuilderFactory factory, int maxEndpoints, SocketAddress address) throws Exception
    {
        final Hashtable<String, String> table = new Hashtable<String, String>();
        table.put("type", JMX_TYPE);
        table.put("name", ObjectName.quote(name));
        final ObjectName oname = new ObjectName(JMX_DOMAIN, table);
        final MBeanServer mbs = ManagementFactory.getPlatformMBeanServer();
        final Listener listener = new Listener()
        {
            @Override
            public void terminate () throws Exception
            {
                mbs.unregisterMBean(oname);
            }
        };
        final RmiServer s = new RmiServer(name, address, factory, maxEndpoints, listener);
        final RmiServerMXBean bean = s.getInstrumentation();
        mbs.registerMBean(bean, oname);
        Acceptor.create(address, s.hub, s.new AcceptorListener());
        return s;
    }

    public RmiServer stop () { hub.stop(); return this; }

    public RmiServer join () throws InterruptedException { hub.join(); return this; }

    public Instrumentation getInstrumentation () { return bean; }

    /**
     * <p>Measures the server.</p>
     *
     * @author erik
     */
    public class Instrumentation implements RmiServerMXBean
    {
        @Override
        public void stop ()
        {
            log.info("Stopping server via JMX.");
            RmiServer.this.stop();
        }

        @Override
        public String getAddress () { return address.toString(); }

        @Override
        public int getOpenConnections ()
        {
            synchronized (endpoints) { return endpoints.size(); }
        }

        @Override
        public int getMaxConnections () { return maxConnections.get(); }

        @Override
        public void setMaxConnections (int n) { maxConnections.set(n); }

        @Override
        public String getStartTime () { return String.format("%tc", new Date(startTime)); }

        @Override
        public String getUptime ()
        {
            final long t = System.currentTimeMillis();
            return String.format("%f seconds", (t - startTime) / 1000.0);
        }

        @Override
        public long getAcceptedConnections () { return hub.getLifetimeConnections(); }

        @Override
        public double getAcceptanceRatio ()
        {
            synchronized (endpoints)
            {
                if (getAcceptedConnections() == 0) { return 0; }
                return (double) getServedConnections() / getAcceptedConnections();
            }
        }

        @Override
        public long getServedConnections ()
        {
            return hub.getConnectionsAccepted();
        }

        @Override
        public long getBytesRead ()
        {
            return hub.getBytesRead();
        }

        @Override
        public long getBytesWritten ()
        {
            return hub.getBytesWritten();
        }

        @Override
        public int getMaxFrameSize () throws IOException
        {
            return maxFrameSize.get();
        }

        @Override
        public void setMaxFrameSize (int n) throws IOException
        {
            maxFrameSize.set(n);
        }

    }

    public static RmiServer main0 (String name, final List<Class<?>> classes, SocketAddress address, int maxConnections) throws Exception
    {
        final EndpointBuilderFactory factory = new EndpointBuilderFactory()
        {
            @Override
            public EndpointBuilder createEndpointBuilder () throws InstantiationException, IllegalAccessException
            {
                final List<Object> objects = new ArrayList<Object>(classes.size());
                for (Class<?> c : classes)
                {
                    final Object o = c.newInstance();
                    objects.add(o);
                }
                return new EndpointBuilder().exportAll(objects);
            }
        };
        return RmiServer.create(name, factory, maxConnections, address);
    }

    public static void main (String[] args)
    {
        try
        {
            final String sName = System.getProperty("name");
            final String sHost = System.getProperty("host");
            final String sPort = System.getProperty("port");
            final String sMaxConnections = System.getProperty("maxConnections");
            if (sName == null) { throw new IllegalArgumentException("missing name"); }
            if (sPort == null) { throw new IllegalArgumentException("missing port"); }
            final int maxConnections = (sMaxConnections == null) ? 1024 : Integer.parseInt(sMaxConnections);
            final int port = Integer.parseInt(sPort);
            final int numClasses = args.length;
            final List<String> classNames = Arrays.asList(args);
            final List<Class<?>> classes = new ArrayList<Class<?>>(numClasses);
            log.info("Loading " + numClasses + " classes...");
            for (String cn : classNames)
            {
                log.info(cn);
                final Class<?> c = Class.forName(cn);
                classes.add(c);
            }
            log.info("Done loading classes.");
            final InetAddress host = InetAddress.getByName(sHost);
            final SocketAddress address = new InetSocketAddress(host, port);
            final RmiServer server = main0(sName, classes, address, maxConnections);
            server.join();
        }
        catch (Exception e)
        {
            log.error("Unhandled exception in main.", e);
        }
    }
}