package id.web.edom.rmi;

public class UnsupportedProtocolVersionException extends ProtocolException
{
    private static final long serialVersionUID = 1L;

    public UnsupportedProtocolVersionException (int version)
    {
        super(String.valueOf(version));
    }
}
