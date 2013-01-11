package id.web.edom.io;

import id.web.edom.Threaded;
import id.web.edom.rmi.Log;

import java.io.EOFException;
import java.io.IOException;
import java.nio.channels.ReadableByteChannel;
import java.nio.channels.SelectableChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.nio.channels.WritableByteChannel;
import java.util.Set;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;

import org.slf4j.Logger;

/**
 * <p>Selects multiple {@link SelectableChannel}s
 * using exactly one {@link Selector}
 * and one {@linkplain Thread thread}.</p>
 *
 * <p>Kills connections that are idle for too long.</p>
 *
 * <p>Uses only one thread and that thread is
 * not the thread used to call the constructor.</p>
 *
 * <p>Also gathers some statistics.</p>
 *
 * @author erik
 */
public class Hub implements Threaded
{

    private static final Logger lt = Log.getTransput();
    private static final Logger ls = Log.getSelector();

    /**
     * <p>The hub expects one instance of this class to be
     * {@linkplain SelectionKey#attachment() attached}
     * to one selection key in the hub.</p>
     *
     * <p>All methods here should return quickly
     * since they run in the thread that is used by the hub
     * to {@linkplain Selector#select() select}.
     * Blocking this thread will hang the entire hub.</p>
     *
     * <p>Throwing an exception will close the associated channel.</p>
     *
     * @author erik
     */
    public static class Listener
    {
        // Does this long really need to be atomic?
        // It is used in only one thread, isn't it?
        /**
         * <p>The time of last access as returned by {@link System#currentTimeMillis()}.</p>
         */
        private final AtomicLong last = new AtomicLong(System.currentTimeMillis());
        /**
         * <p>Updates the time of last access.</p>
         */
        private final void touch () { last.set(System.currentTimeMillis()); }
        /**
         * <p>Gets the time of last access.
         *
         * @return the time of last access
         */
        private final long getLastAccess () { return last.get(); }
        /**
         * <p>If this method returns true,
         * the hub will close the channel
         * if the channel is idle for too long.</p>
         *
         * @return whether the channel is subject to idle timeout
         */
        public boolean canTimeOut () { return true; }
        /**
         * <p>The default implementation closes the channel.</p>
         *
         * @param channel the channel
         *
         * @throws Exception if there is an error
         */
        public void terminate (SelectableChannel channel) throws Exception
        {
            Log.close(channel);
            channel.close();
        }
        /**
         *
         * @param in
         * @return the number of bytes read
         * @throws Exception
         */
        public int read (ReadableByteChannel in) throws Exception { return 0; }
        /**
         *
         * @param out
         * @return the number of bytes actually written
         * @throws Exception
         */
        public int write (WritableByteChannel out) throws Exception { return 0; }
        public void accept (ServerSocketChannel channel) throws Exception {}
        public void connect (SocketChannel channel) throws Exception {}
    }

    /**
     * <p>Milliseconds.</p>
     */
    private final long maxIdleDuration = 5 * 60 * 1000;

    /**
     * <p>The soonest time to kill idle connections.</p>
     */
    private long nextIdleCheck = System.currentTimeMillis();

    /**
     * <p>Set this flag to true to signal the {@link #thread} to stop
     * as soon as it reads this flag.</p>
     */
    private final AtomicBoolean shutdown = new AtomicBoolean();
    private final AtomicLong lifetimeConnections = new AtomicLong();
    private final AtomicLong connectionsAccepted = new AtomicLong();
    private final AtomicLong bytesRead = new AtomicLong();
    private final AtomicLong bytesWritten = new AtomicLong();
    private final Object lock = new Object();
    private final Selector selector;
    private final Thread thread;

    private Hub () throws Exception
    {
        lt.info("Starting {}.", this);
        this.selector = Selector.open();
        this.thread = new Thread ()
        {
            @Override
            public void run ()
            {
                Hub.this.run();
            }
        };
    }

    /**
     * <p>Worker thread loop.</p>
     */
    private void run ()
    {
        try
        {
            lt.info("{} started.", this);
            for (;;)
            {
                if (shutdown.get()) { break; }
                final long now = System.currentTimeMillis();
                final long duration = nextIdleCheck - now;
                if (duration <= 0)
                {
                    killIdleChannels();
                    continue;
                }
                synchronized (lock) {}
                selector.select(duration);
                dispatchSelectedChannels();
            }
        }
        catch (Exception e)
        {
            lt.error("Unhandled exception in hub loop.", e);
        }
        finally
        {
            lt.info("Stopping {}.", this);
            for (SelectionKey k : selector.keys())
            {
                if (!k.isValid()) { continue; } // key is already cancelled
                cancel(k);
            }
            try
            {
                selector.close();
            }
            catch (Exception e)
            {
                lt.error("Exception while closing selector.", e);
            }
            lt.info("{} stopped.", this);
        }
    }
    /**
     * <p>Kills all connections that have been idle
     * for more than {@link #maxIdleDuration} milliseconds.</p>
     */
    private void killIdleChannels ()
    {
        final long now = System.currentTimeMillis();
        long minLastAccess = now;
        synchronized (lock)
        {
            for (SelectionKey key : selector.keys())
            {
                final Listener l = (Listener) key.attachment();
                if (!l.canTimeOut()) { continue; }
                final long lastAccess = l.getLastAccess();
                final long idleTime = now - lastAccess;
                if (idleTime >= maxIdleDuration)
                {
                    Log.timeout(key.channel());
                    cancel(key);
                    continue;
                }
                if (lastAccess < minLastAccess)
                {
                    minLastAccess = lastAccess;
                }
            }
        }
        nextIdleCheck = minLastAccess + maxIdleDuration;
    }

    /**
     * <p>Dispatch events to listeners of selected channels.</p>
     */
    private void dispatchSelectedChannels ()
    {
        final Set<SelectionKey> selectedKeys = selector.selectedKeys();
        if (ls.isTraceEnabled())
        {
            final int n = selectedKeys.size();
            if (n > 0)
            {
                ls.trace("Selected {} keys.", n);
            }
        }
        for (SelectionKey key : selectedKeys)
        {
            try
            {
                if (ls.isTraceEnabled())
                {
                    ls.trace("{} readyOps:{}.", key, key.readyOps());
                }
                final Listener l = (Listener) key.attachment();
                final SelectableChannel c = key.channel();
                l.touch();
                if (key.isReadable())
                {
                    final int n = l.read((ReadableByteChannel) c);
                    bytesRead.addAndGet(n);
                }
                if (key.isWritable())
                {
                    final int n = l.write((WritableByteChannel) c);
                    bytesWritten.addAndGet(n);
                }
                if (key.isAcceptable())
                {
                    lifetimeConnections.incrementAndGet();
                    l.accept((ServerSocketChannel) c);
                    connectionsAccepted.incrementAndGet();
                }
                if (key.isConnectable())
                {
                    l.connect((SocketChannel) c);
                }
            }
            catch (EOFException e)
            {
                lt.info("{} closed by peer.", key.channel());
                cancel(key);
            }
            catch (Exception e)
            {
                lt.error("Exception while dispatching events in selected channel.", e);
                cancel(key);
            }
        }
        selectedKeys.clear();
    }

    /**
     * <p>This method is safe to call at any time from any thread.
     * Calls {@link Listener#terminate(SelectableChannel)}.</p>
     *
     * @param key the key to cancel
     *
     * @return this
     */
    public Hub cancel (SelectionKey key)
    {
        try
        {
            final Listener l = (Listener) key.attachment();
            final SelectableChannel c = key.channel();
            l.terminate(c);
        }
        catch (Exception e)
        {
            lt.warn("Exception while cancelling key " + key + ".", e);
        }
        finally
        {
            key.cancel();
        }
        return this;
    }

    /**
     * <p>Registers the channel for use with the selector in this hub.</p>
     *
     * @param channel channel
     * @param operations operations
     * @param listener listener
     *
     * @return the selection key
     *
     * @throws IOException if there is a transput error
     *
     * @see {@link SelectableChannel#register(Selector, int, Object)}
     */
    public SelectionKey register (SelectableChannel channel, int operations, Listener listener) throws IOException
    {
        if (shutdown.get()) { throw new IllegalStateException(); }
        if (ls.isDebugEnabled())
        {
            ls.debug("Registering {} interestOps:{}.", channel, operations);
        }
        final SelectionKey key;
        synchronized (lock)
        {
            selector.wakeup();
            channel.configureBlocking(false);
            key = channel.register(selector, operations, listener);
        }
        return key;
    }

    public Hub stop ()
    {
        shutdown.set(true);
        selector.wakeup();
        return this;
    }

    public Hub join () throws InterruptedException
    {
        thread.join();
        return this;
    }

    public void setInterestOps (SelectionKey key, int newOps)
    {
        if (ls.isTraceEnabled())
        {
            final int oldOps = key.interestOps();
            if (oldOps != newOps)
            {
                ls.trace("{} interestOps:{}->{}.", key, oldOps, newOps);
            }
        }
        synchronized (lock)
        {
            selector.wakeup();
            key.interestOps(newOps);
        }
    }

    private static final AtomicLong serial = new AtomicLong();

    public static Hub create () throws Exception
    {
        final Hub m = new Hub();
        m.thread.setName("hub-" + serial.getAndIncrement());
        m.thread.start();
        return m;
    }

    public long getLifetimeConnections () { return lifetimeConnections.get(); }
    public long getConnectionsAccepted () { return connectionsAccepted.get(); }
    public long getBytesRead () { return bytesRead.get(); }
    public long getBytesWritten () { return bytesWritten.get(); }
}
