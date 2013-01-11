package id.web.edom.rmi;

import id.web.edom.rmi.Frame;
import id.web.edom.rmi.InvalidStatusNumberException;
import id.web.edom.rmi.RemoteException;
import id.web.edom.rmi.SerDes;
import id.web.edom.rmi.Status;
import id.web.edom.rmi.TimeoutException;

class DefaultReturnFrameHandler implements FrameHandler
{
    private final Object lock = new Object();
    private final Class<?> returnType;
    private final long timeout;
    private final SerDes serdes = SerDes.getDefault();
    private boolean returned;
    private Object ret;
    private boolean thrw;

    /**
     *
     * @param returnType
     * @param timeout number of milliseconds to wait for a return frame
     */
    public DefaultReturnFrameHandler (Class<?> returnType, long timeout)
    {
        this.returnType = returnType;
        this.timeout = timeout;
    }

    @Override
    public void handle (Frame f) throws Exception
    {
        synchronized (lock)
        {
            f.seekArguments();
            final int status = f.getStatusNumber();
            switch (status)
            {
            case Status.OK:
                ret = serdes.read(returnType, f);
                break;
            case Status.ERROR:
                ret = serdes.read(Throwable.class, f);
                thrw = true;
                break;
            default:
                ret = new InvalidStatusNumberException(status);
                thrw = true;
                break;
            }
            returned = true;
            lock.notifyAll();
        }
    }

    /**
     *
     * @return
     * @throws TimeoutException
     * @throws Exception
     */
    public Object get() throws Exception
    {
        final long t0 = System.currentTimeMillis();

        final long t1 = t0 + timeout;
        synchronized (lock)
        {
            while (!returned)
            {
                if (System.currentTimeMillis() > t1) throw new TimeoutException();
                lock.wait(timeout);
            }
            if (thrw) { throw new RemoteException((Throwable) ret); }
            return ret;
        }
    }
}
