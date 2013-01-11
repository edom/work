package id.web.edom.rmi.test;

import id.web.edom.rmi.Frame;
import id.web.edom.rmi.FrameHandler;
import id.web.edom.rmi.test.back.FakeEndpoint;
import id.web.edom.rmi.test.back.LoopbackTest;

/**
 * The fake endpoint reads from p0 and writes to p1.
 * 
 * @author erik
 */
public class TestWithFakeEndpoint extends LoopbackTest
{
    protected FakeEndpoint fake;
    protected FrameHandler handler;
    
    @Override
    public void before () throws Exception
    {
        super.before();
        handler = null;
        fake = new FakeEndpoint(p0.source(), p1.sink())
        {
            @Override
            public void handle (Frame f) throws Exception
            {
                handler.handle(f);
            }
        };
    }

    @Override
    public void after () throws Exception
    {
        fake.shutdown();
        fake.join();
        handler = null;
        super.after();
    }
    
}
