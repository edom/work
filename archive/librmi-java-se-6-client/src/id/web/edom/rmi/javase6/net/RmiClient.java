package id.web.edom.rmi.javase6.net;

import id.web.edom.Threaded;
import id.web.edom.io.Hub;
import id.web.edom.rmi.Endpoint;
import id.web.edom.rmi.EndpointBuilder;

import java.io.IOException;
import java.net.Socket;
import java.net.SocketAddress;
import java.nio.channels.SelectableChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.SocketChannel;

public class RmiClient implements Threaded
{
    public static class Listener
    {
        public void connect (Endpoint endpoint) throws Exception {}
    }

    private final SocketChannel channel;
    private final Hub hub;
    private final Listener listener;

    private class HubListener extends Hub.Listener
    {
        @Override
        public void terminate (SelectableChannel channel) throws Exception
        {
            try { hub.stop(); }
            finally { super.terminate(channel); }
        }

        @Override
        public void connect (SocketChannel channel) throws Exception
        {
            if (!channel.finishConnect()) { throw new IOException(); }
            final Endpoint endpoint = new EndpointBuilder().setHub(hub).setInput(channel).setOutput(channel).createEndpoint();
            listener.connect(endpoint);
        }
    }

    private RmiClient (SocketAddress remote, Listener listener) throws Exception
    {
        this.listener = listener;
        this.hub = Hub.create();
        this.channel = SocketChannel.open();
        final Socket s = channel.socket();
        s.setReuseAddress(true);
        s.setTcpNoDelay(true);
    }

    public static RmiClient create (SocketAddress remote, Listener listener) throws Exception
    {
        final RmiClient client = new RmiClient(remote, listener);
        final SocketChannel channel = client.channel;
        client.hub.register(channel, SelectionKey.OP_CONNECT, client.new HubListener());
        channel.connect(remote);
        return client;
    }

    @Override
    public RmiClient stop ()
    {
        hub.stop();
        return this;
    }

    @Override
    public RmiClient join () throws InterruptedException
    {
        hub.join();
        return this;
    }


}
