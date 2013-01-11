package id.web.edom.rmi;

/**
 * <p>Thrown when there is no {@linkplain Transformer transformer}
 * for a requested type.</p>
 *
 * @author erik
 *
 * @see SerDes
 * @see Transformer
 */
public class NoSuchTransformerException extends RmiException
{
    private static final long serialVersionUID = 1L;

    /**
     * @param type the requested type that caused this exception
     */
    public NoSuchTransformerException (Class type)
    {
        super("for this type: " + type.getName());
    }
}
