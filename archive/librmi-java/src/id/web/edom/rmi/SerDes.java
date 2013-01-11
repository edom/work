package id.web.edom.rmi;

import id.web.edom.rmi.io.PrimitiveInput;
import id.web.edom.rmi.io.PrimitiveOutput;
import id.web.edom.rmi.transformers.ClassTransformer;
import id.web.edom.rmi.transformers.IntTransformer;
import id.web.edom.rmi.transformers.MethodDescriptionTransformer;
import id.web.edom.rmi.transformers.PrimitiveByteArrayTransformer;
import id.web.edom.rmi.transformers.StringTransformer;
import id.web.edom.rmi.transformers.VoidTransformer;

import java.util.Hashtable;

/**
 * <p>Serializes or deserializes objects.</p>
 *
 * <p>All instances of this class are thread-safe.</p>
 *
 * @author erik
 */
public final class SerDes
{
    private static final SerDes instance = new SerDes();
    private final Hashtable map = new Hashtable();
    private final Object lock = new Object();

    private SerDes ()
    {
        // These are the default transformers.
        // This list might change in the future.
        final Transformer[] defs = new Transformer[]
        {
            new ClassTransformer(),
            new IntTransformer(),
            new MethodDescriptionTransformer(),
            new PrimitiveByteArrayTransformer(),
            new StringTransformer(),
            new VoidTransformer()
        };
        // Register all the above default transformers.
        synchronized (lock)
        {
            for (int i = 0; i < defs.length; ++i)
            {
                final Transformer t = defs[i];
                final Class[] types = t.getTypes();
                for (int j = 0; j < types.length; ++j)
                {
                    final Class type = types[j];
                    final Object old = map.put(type, t);
                    if (old != null)
                    {
                        throw new RuntimeException(
                            "duplicate transformer for " + type.getName() + ": " +
                        old.getClass().getName() + " and " + t.getClass().getName() + "\n");
                    }
                }
            }
        }
    }

    /**
     * <p>Gets the default instance.</p>
     *
     * @return the default instance
     */
    public static SerDes getDefault () { return instance; }

    /**
     * <p>Registers the transformer for the type.</p>
     *
     * @param type the type
     * @param transformer the transformer
     *
     * @throws RuntimeException if there is already a transformer for that type
     *
     * @return this
     */
    public SerDes add (Class type, Transformer transformer)
    {
        synchronized (lock)
        {
            if (map.put(type, transformer) != null)
            {
                throw new RuntimeException("a serdes already exists for "
                    + type.getName());
            }
        }
        return this;
    }

    /**
     * <p>Reconstructs an instance of the type from the stream.</p>
     *
     * @param type the type
     * @param r the stream
     *
     * @return an instance of the type
     *
     * @throws NoSuchTransformerException if there is no transformer for that type
     * @throws Exception
     */
    public Object read (Class type, PrimitiveInput r) throws NoSuchTransformerException, Exception
    {
        return get(type).read(r);
    }

    /**
     * <p>Writes a representation of the object of the type
     * to the stream.</p>
     *
     * @param type the type of the object
     * @param o must be an instance of the type
     * @param w the stream
     *
     * @return this
     *
     * @throws NoSuchTransformerException if there is no transformer for that type
     * @throws Exception any other error
     */
    public SerDes write (Class type, Object o, PrimitiveOutput w) throws NoSuchTransformerException, Exception
    {
        get(type).write(o, w);
        return this;
    }

    /**
     * <p>Gets the transformer for the type.
     * When no such transformer is found,
     * this method throws an exception instead of returning null.</p>
     *
     * @param type the type
     *
     * @return the transformer for the type
     *
     * @throws NoSuchTransformerException if there is no transformer for that type
     */
    private Transformer get (Class type) throws NoSuchTransformerException
    {
        final Transformer s;
        synchronized (lock)
        {
            s = (Transformer) map.get(type);
        }
        if (s == null) throw new NoSuchTransformerException(type);
        return s;
    }
}
