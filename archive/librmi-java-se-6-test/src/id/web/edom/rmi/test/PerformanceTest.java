package id.web.edom.rmi.test;

import id.web.edom.rmi.EndpointBuilder;
import id.web.edom.rmi.Java6Endpoint;
import id.web.edom.rmi.test.back.Echo;
import id.web.edom.rmi.test.back.EchoImpl;

import java.nio.channels.Pipe;

import org.junit.Test;

public class PerformanceTest
{
    @Test
    public void both () throws Exception
    {
        final Pipe cs = Pipe.open();
        final Pipe sc = Pipe.open();
        cs.sink().configureBlocking(false);
        cs.source().configureBlocking(false);
        sc.sink().configureBlocking(false);
        sc.source().configureBlocking(false);
        final Object back = new EchoImpl();
        final Java6Endpoint s = new EndpointBuilder().export(back).setInput(cs.source()).setOutput(sc.sink()).createEndpoint();
        final Java6Endpoint c = new EndpointBuilder().setInput(sc.source()).setOutput(cs.sink()).createEndpoint();
        final Echo a = (Echo) c.wrapRemote(0, new Class<?>[]{Echo.class});
        final byte[] message = new byte[128];
        final int n = 32768;
        final long t0 = System.nanoTime();
        for (int i = 0; i < n; ++i)
        {
            a.echo(message);
        }
        final long t1 = System.nanoTime();
        final double dt = (t1 - t0) / 1.0e9;
        final long nb = (long) n * message.length;
        final double tps = n / dt;
        final double bps = nb / dt;
        System.out.format("transactions/second = %f (%d/%f)\n", tps, n, dt);
        System.out.format("bytes/second        = %f (%d/%f)\n", bps, nb, dt);
        s.shutdown();
        s.join();
        c.join();
    }

}
