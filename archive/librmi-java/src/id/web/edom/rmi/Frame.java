package id.web.edom.rmi;

import id.web.edom.rmi.io.PrimitiveInput;
import id.web.edom.rmi.io.PrimitiveOutput;

/**
 * <p>A frame is exchanged between two endpoints.</p>
 *
 * @author erik
 */
public interface Frame extends PrimitiveInput, PrimitiveOutput
{
    /**
     * <p>True if this frame is a call;
     * false if this frame is a return.</p>
     *
     * @return whether this frame is a call
     */
    boolean isCall ();

    /**
     * <p>The length is the first four bytes of the frame
     * as an integer in big-endian byte order.
     * The length specifies the number of bytes
     * in the rest of the frame.</p>
     *
     * @return the number of remaining bytes
     */
    int getLength ();

    int getSequenceNumber ();

    int getMethodNumber ();

    int getStatusNumber ();

    int getObjectNumber ();

    /**
     * @param s the new sequence number
     * @return this
     */
    Frame setSequenceNumber (int s);

    /**
     *
     * @param s the new method number
     * @return this
     */
    Frame setMethodNumber (int s);

    /**
     *
     * @param s the new status number
     * @return this
     */
    Frame setStatusNumber (int s);

    /**
     *
     * @param o the new object number
     * @return this
     */
    Frame setObjectNumber (int o);

    /**
     * <p>Sets the {@linkplain #getLength() length}
     * of this frame to the {@linkplain #position() position}.</p>
     *
     * @return this
     */
    Frame computeLength ();

    /**
     * <p>Resets the {@linkplain #position() position} to zero
     * and resets the {@linkplain #limit(int) limit} to the {@linkplain #capacity() capacity}
     * without changing the content of this frame.</p>
     *
     * @return this
     */
    Frame reset ();

    /**
     * <p>Sets the {@linkplain #position() position} of this frame
     * to the beginning of the arguments (that is the end of the header).</p>
     *
     * @return this
     */
    Frame seekArguments ();

    /**
     * <p>Gets the position.</p>
     *
     * @return the position
     */
    int position ();

    /**
     *
     * @param p
     * @return this
     */
    Frame position (int p);

    int limit ();

    /**
     * <p>Sets the index of the byte after the last byte of this frame.</p>
     *
     * @param i one plus the index of the last byte
     * @return this
     */
    Frame limit (int i);

    /**
     * <p>The maximum number of bytes that can fit in this frame.</p>
     *
     * @return the capacity
     */
    int capacity ();

    /**
     * <p>The number of bytes from the {@linkplain #position() position} of the frame
     * to the {@linkplain #limit(int) limit} of the frame.</p>
     *
     * @return the number of remaining bytes
     */
    int remaining ();

    /**
     * <p>The bytes from the start to the {@linkplain #limit(int) limit} of this frame
     * as an array of bytes.</p>
     *
     * <p>It is unspecified whether modifying the array
     * will also modify the content of this frame.</p>
     *
     * @return the byte array
     */
    byte[] toByteArray ();
}
