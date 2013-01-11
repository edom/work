package id.web.edom.rmi.test.back;

/**
 * This defines the interface.
 * The server exports an object that implements this interface.
 * The client uses this interface to remotely invoke the methods of that object.
 * @author erik
 */
public interface Echo
{
    byte[] echo (byte[] b);
}
