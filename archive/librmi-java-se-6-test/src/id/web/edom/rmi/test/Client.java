package id.web.edom.rmi.test;

import id.web.edom.rmi.Endpoint;
import id.web.edom.rmi.EndpointBuilder;
import id.web.edom.rmi.Frame;
import id.web.edom.rmi.FrameHandler;

import org.junit.Test;

/**
 * Test the correctness of the client.
 * @author erik
 */
public class Client extends TestWithFakeEndpoint
{
    protected Endpoint client;
    
    @Override
    public void before () throws Exception
    {
        super.before();
        client = new EndpointBuilder().setInput(p1.source()).setOutput(p0.sink()).setTimeout(500).createEndpoint();
    }

    @Override
    public void after () throws Exception
    {
        client.shutdown();
        client.join();
        super.after();
    }

    @Test
    public void a () throws Throwable
    {
        handler = new FrameHandler()
        {
            @Override
            public void handle (Frame f) throws Exception
            {
                System.err.println(f.toString());
            }
        };
        client.remoteInvoke(0, 0, void.class, new Object[0], new Class[0]);
    }
}
