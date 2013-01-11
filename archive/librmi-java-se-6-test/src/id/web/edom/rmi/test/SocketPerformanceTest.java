package id.web.edom.rmi.test;

import id.web.edom.rmi.Endpoint;
import id.web.edom.rmi.EndpointBuilder;
import id.web.edom.rmi.Java6Endpoint;
import id.web.edom.rmi.test.back.Echo;
import id.web.edom.rmi.test.back.EchoImpl;

import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketAddress;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;

import org.junit.Test;

public class SocketPerformanceTest
{
    private boolean bound;
    private SocketAddress address;
    @Test
    public void both () throws Exception
    {
        final Object sync = new Object();
        final Thread t = new Thread()
        {
            @Override
            public void run ()
            {
                try
                {
                    final ServerSocketChannel c = ServerSocketChannel.open();
                    final ServerSocket s = c.socket();
                    s.setReuseAddress(true);
                    s.setPerformancePreferences(2, 1, 0);
                    s.bind(null);
                    synchronized (sync)
                    {
                        bound = true;
                        address = s.getLocalSocketAddress();
                        sync.notifyAll();
                    }
                    final SocketChannel z = c.accept();
                    final Object back = new EchoImpl();
                    final Endpoint ns = new EndpointBuilder().export(back).setInput(z).setOutput(z).createEndpoint();
                    ns.join();
                    z.close();
                    c.close();
                }
                catch (Exception e)
                {
                    e.printStackTrace();
                }
            }

        };
        t.start();
        synchronized (sync)
        {
            while (!bound)
            {
                sync.wait();
            }
        }
        final byte[] message = new byte[16384];
        final int n = 32768;
        final SocketChannel sc = SocketChannel.open();
        final Socket s = sc.socket();
        s.setPerformancePreferences(0, 2, 1);
        s.setReuseAddress(true);
        s.setTcpNoDelay(true);
        s.connect(address);
        final Java6Endpoint y = new EndpointBuilder().setInput(sc).setOutput(sc).createEndpoint();
        final Echo a = (Echo) y.wrapRemote(0, new Class<?>[]{Echo.class});
        final long t0 = System.nanoTime();
        for (int i = 0; i < n; ++i)
        {
            a.echo(message);
        }
        final long t1 = System.nanoTime();
        y.shutdown();
        y.join();
        final double dt = (t1 - t0) / 1.0e9;
        final long nb = (long) n * message.length;
        final double tps = n / dt;
        final double bps = nb / dt;
        System.out.format("transactions/second = %f (%d/%f)\n", tps, n, dt);
        System.out.format("throughput          = %f (%d/%f) B/s = %f MiB/s\n", bps, nb, dt, bps / 1048576);
    }

}
