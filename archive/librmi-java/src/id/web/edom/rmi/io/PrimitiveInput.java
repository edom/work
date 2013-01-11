package id.web.edom.rmi.io;

/**
 * <p>Input stream of primitive values.</p>
 *
 * <p>This interface is similar to {@link java.io.DataInput}.</p>
 *
 * @author erik
 */
public interface PrimitiveInput
{
    /**
     * <p>Reads the next four bytes as an integer
     * in big-endian byte order.</p>
     *
     * @return the integer
     */
    int readInt ();

    /**
     * <p>Reads the next bytes into the array until the array is full.</p>
     *
     * @param b where to store the bytes
     *
     * @throws RuntimeException
     * if there is not enough bytes in this stream to fill up the array
     */
    void readFully (byte[] b);
}
