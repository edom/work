package id.web.edom.rmi;

/**
 * Method numbers of special methods.
 * @author erik
 */
public final class SpecialMethods
{
    private SpecialMethods () {}
    /**
     * Looks up methods
     * (translates method signature into method number).
     */
    public static final int LOOKUP = 0xFFFE;
    public static final int GET_REMOTE_INTERFACE = 0xFFFF;
}
