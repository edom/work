package id.web.edom.rmi.test.back;

import id.web.edom.rmi.EndpointBuilder;
import id.web.edom.rmi.Java6Endpoint;

import java.io.IOException;
import java.nio.channels.Pipe;

import org.junit.After;
import org.junit.Before;

public class LoopbackTestWithTwoEndpoints
{
    protected Java6Endpoint s;
    protected Java6Endpoint c;

    protected Pipe p0;
    protected Pipe p1;

    @Before
    public void before () throws Exception
    {
        p0 = Pipe.open();
        p1 = Pipe.open();
        final Object back = new EchoImpl();
        s = new EndpointBuilder().export(back).setTimeout(250).setInput(p0.source()).setOutput(p1.sink()).createEndpoint();
        c = new EndpointBuilder().setTimeout(250).setInput(p1.source()).setOutput(p0.sink()).createEndpoint();
    }

    @After
    public void after () throws Exception
    {
        c.shutdown();
        s.shutdown();
        c.join();
        s.join();
        p0.sink().close();
        p0.source().close();
        p1.sink().close();
        p1.source().close();
    }
}
