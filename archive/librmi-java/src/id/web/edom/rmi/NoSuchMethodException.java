package id.web.edom.rmi;

public class NoSuchMethodException extends RmiException
{
    private static final long serialVersionUID = 1L;
    public NoSuchMethodException (String name)
    {
        super(name);
    }
}
