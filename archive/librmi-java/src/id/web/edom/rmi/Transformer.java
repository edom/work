package id.web.edom.rmi;

import id.web.edom.rmi.io.PrimitiveInput;
import id.web.edom.rmi.io.PrimitiveOutput;

/**
 * <p>Serializes or deserializes objects of certain types.</p>
 *
 * <p>{@linkplain #write(Object, PrimitiveOutput) Serialization}
 * transforms an object into bytes
 * (that can be sent over network, for example)
 * whereas
 * {@linkplain #read(PrimitiveInput) deserialization}
 * reconstructs an object from such bytes.</p>
 *
 * <p>This transformer can only work with
 * the given {@linkplain #getTypes() types}.</p>
 *
 * @author erik
 */
public interface Transformer
{
    /**
     * <p>Gets the types which are handled by this transformer.</p>
     *
     * @return the types which are handled by this transformer
     */
    Class[] getTypes ();

    /**
     * <p>Reconstructs an instance of a type handled by this transformer.</p>
     *
     * @param r where the object is to be read from
     *
     * @return this
     *
     * @throws Exception
     */
    Object read (PrimitiveInput r) throws Exception;

    /**
     * <p>Write the instance to the stream so that
     * the instance can be {@linkplain #read(PrimitiveInput) reconstructed}
     * on the other side.</p>
     *
     * @param o the object to be written
     * @param w where the object is to be written to
     *
     * @return this
     *
     * @throws Exception
     */
    Transformer write (Object o, PrimitiveOutput w) throws Exception;
}
