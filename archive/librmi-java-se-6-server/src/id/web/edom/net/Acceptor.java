package id.web.edom.net;

import id.web.edom.io.Hub;

import java.net.ServerSocket;
import java.net.SocketAddress;
import java.nio.channels.SelectableChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.ServerSocketChannel;

/**
 * <p>Opens a socket that accepts incoming connections.</p>
 *
 * @author erik
 */
public final class Acceptor
{
    /**
     * <p>Each method of this class should return quickly
     * and should be thread-safe.</p>
     *
     * @author erik
     */
    public static class Listener
    {
        /**
         * <p>Called when the acceptor accepts a new connection.</p>
         *
         * <p>If this method throws any exception,
         * the caller closes the socket right away.
         * Otherwise if this method returns normally,
         * the caller gives the socket to the callee.
         * The callee thus must eventually close the socket somehow.</p>
         *
         * <p>Throwing an {@link InterruptedException} terminates the acceptor.</p>
         *
         * @param c the server socket channel
         *
         * @throws Exception
         */
        public void accept (ServerSocketChannel c) throws Exception { throw new UnsupportedOperationException("this method must be overridden"); }
        /**
         * <p>Called when the acceptor is stopping.</p>
         */
        public void terminate () throws Exception {};
    }

    private class HubListener extends Hub.Listener
    {
        @Override
        public boolean canTimeOut ()
        {
            return false;
        }
        @Override
        public void accept (ServerSocketChannel channel) throws Exception
        {
            listener.accept(channel);
        }
        @Override
        public void terminate (SelectableChannel channel) throws Exception
        {
            try { listener.terminate(); }
            finally { super.terminate(channel); }
        }
    }

    private final ServerSocketChannel channel;
    private final Hub hub;
    private final Listener listener;
    private SelectionKey key;

    private Acceptor (SocketAddress address, Hub hub, Listener listener) throws Exception
    {
        this.hub = hub;
        this.listener = listener;
        this.channel = ServerSocketChannel.open();
        final ServerSocket s = channel.socket();
        // Prefer low latency to high bandwidth to short connection time.
        s.setPerformancePreferences(0, 2, 1);
        s.setReuseAddress(true);
        s.bind(address);
    }

    /**
     * @param address the address to which the socket will be bound; can be null
     * @param hub the hub for non-blocking transput
     * @param listener what to do when a connection is accepted; cannot be null
     *
     * @throws Exception if there is an error
     *
     * @see {@link ServerSocket#accept()}
     */
    public static Acceptor create (SocketAddress address, Hub hub, Listener listener) throws Exception
    {
        if (listener == null) { throw new IllegalArgumentException("listener cannot be null"); }
        final Acceptor s = new Acceptor(address, hub, listener);
        s.key = hub.register(s.channel, SelectionKey.OP_ACCEPT, s.new HubListener());
        return s;
    }

    /**
     * <p>It is safe to call this at any time from any thread.</p>
     *
     * @return
     * @throws Exception
     */
    public Acceptor close () throws Exception
    {
        hub.cancel(key);
        return this;
    }

    public SocketAddress getLocalSocketAddress ()
    {
        return channel.socket().getLocalSocketAddress();
    }
}
