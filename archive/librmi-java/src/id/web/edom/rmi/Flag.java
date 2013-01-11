package id.web.edom.rmi;


/**
 * <p>Bit masks for {@link Frame} flags.</p>
 *
 * @author erik
 *
 */
public final class Flag
{
    private Flag () {}
    public static final int MASK_TYPE = 0x80;
    public static final int TYPE_CALL = 0x00;
    public static final int TYPE_RETURN = 0x80;
}
