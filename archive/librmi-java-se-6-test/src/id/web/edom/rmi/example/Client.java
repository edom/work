package id.web.edom.rmi.example;

import id.web.edom.rmi.Endpoint;
import id.web.edom.rmi.Java6Endpoint;
import id.web.edom.rmi.javase6.net.RmiClient;

import java.net.InetSocketAddress;

public class Client
{
    private static Endpoint endpoint;

    public static void main (String[] args) throws Exception
    {
        final Object lock = new Object();
        final RmiClient client;
        synchronized (lock)
        {
            client = RmiClient.create(new InetSocketAddress("localhost", Server.PORT), new RmiClient.Listener()
            {
                @Override
                public void connect (Endpoint endpoint) throws Exception
                {
                    synchronized (lock)
                    {
                        Client.endpoint = endpoint;
                        lock.notifyAll();
                    }
                }
            });
            lock.wait();
        }
        final Java6Endpoint e = (Java6Endpoint) endpoint;
        final EchoServletRemote s = (EchoServletRemote) e.wrapRemote(0, new Class<?>[] { EchoServletRemote.class });
        final byte[] b = new byte[8];
        for (int i = 0; i < 1; ++i)
        {
            s.echo(b);
        }
        client.stop();
        client.join();
    }
}
