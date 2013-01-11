package id.web.edom.rmi;

/**
 * <p>Wraps an exception thrown at the other endpoint.
 * The wrapped exception is stored as the cause of this exception.</p>
 *
 * @author erik
 *
 */
public class RemoteException extends RmiException
{
    private static final long serialVersionUID = 1L;

    public RemoteException ()
    {
        super();
    }

    public RemoteException (String message, Throwable cause)
    {
        super(message, cause);
    }

    public RemoteException (String message)
    {
        super(message);
    }

    public RemoteException (Throwable cause)
    {
        super(cause);
    }

}
