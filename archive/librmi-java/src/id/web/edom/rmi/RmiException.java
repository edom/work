package id.web.edom.rmi;

/**
 * <p>Superclass for all exceptions in this package.</p>
 *
 * @author erik
 */
public class RmiException extends Exception
{
    private static final long serialVersionUID = 1L;

    public RmiException ()
    {
        super();
    }

    public RmiException (String message, Throwable cause)
    {
        super(message, cause);
    }

    public RmiException (String message)
    {
        super(message);
    }

    public RmiException (Throwable cause)
    {
        super(cause);
    }
}
