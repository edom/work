package id.web.edom.rmi.io;

/**
 * <p>Output stream of primitive values.</p>
 *
 * <p>This interface is similar to {@link java.io.DataOutput}.</p>
 *
 * @author erik
 */
public interface PrimitiveOutput
{
    /**
     * <p>Writes the integer as four bytes in big-endian byte order into this stream.</p>
     *
     * @param i the integer
     */
    void writeInt (int i);

    /**
     * <p>Writes the entire byte array into this stream.</p>
     *
     * @param b the byte array
     */
    void write (byte[] b);
}
