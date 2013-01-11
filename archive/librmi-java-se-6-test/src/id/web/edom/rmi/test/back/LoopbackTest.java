package id.web.edom.rmi.test.back;

import java.nio.channels.Pipe;

import org.junit.After;
import org.junit.Before;

public class LoopbackTest
{
    protected Pipe p0;
    protected Pipe p1;

    @Before
    public void before () throws Exception
    {
        p0 = Pipe.open();
        p1 = Pipe.open();
    }

    @After
    public void after () throws Exception
    {
        p0.sink().close();
        p0.source().close();
        p1.sink().close();
        p1.source().close();
    }
}
