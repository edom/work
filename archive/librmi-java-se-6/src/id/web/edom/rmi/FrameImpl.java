package id.web.edom.rmi;

import id.web.edom.rmi.Flag;
import id.web.edom.rmi.Frame;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.WritableByteChannel;

//FIXME use nio CharsetDecoder instead of new String

/**
 * <p>Instances of this class are not thread-safe.</p>
 *
 * @author erik
 */
public class FrameImpl implements Frame
{
    private final ByteBuffer buffer;

    FrameImpl (int capacity)
    {
        this.buffer = ByteBuffer.allocate(capacity);
    }

    public int getLength () { return buffer.getInt(0); }
    public int getSequenceNumber () { return buffer.getInt(4); }
    private int getFlags () { return buffer.get(8) & 0xFF; }
    public int getVersion () { return buffer.get(9) & 0xFF; }
    public int getMethodNumber () { return (buffer.getShort(10) & 0xFFFF); }
    public int getStatusNumber () { return (buffer.getShort(10) & 0xFFFF); }
    public int getObjectNumber () { return buffer.getInt(12); }

    public FrameImpl setSequenceNumber (int s) { buffer.putInt(4, s); return this; }
    private FrameImpl setFlags (int f) { buffer.put(8, (byte) f); return this; }
    public FrameImpl setMethodNumber (int s) { buffer.putShort(10, (short) s); setFlags(Flag.TYPE_CALL); return this; }
    public FrameImpl setStatusNumber (int s) { buffer.putShort(10, (short) s); setFlags(Flag.TYPE_RETURN); return this; }
    public FrameImpl setObjectNumber (int o) { buffer.putInt(12, o); return this; }

    public boolean isCall () { return ((getFlags() & Flag.MASK_TYPE) == Flag.TYPE_CALL); }
    public boolean isReturn () { return ((getFlags() & Flag.MASK_TYPE) == Flag.TYPE_RETURN); }


    public void writeInt (int arg0) { buffer.putInt(arg0); }

    public int readInt () { return buffer.getInt(); }
    public void readFully (byte[] b) { buffer.get(b); }

    public void write (byte[] b) { buffer.put(b); }

    private static final int ARGUMENTS_START = 16;
    /**
     * Set the buffer position to the start of the arguments.
     */
    public FrameImpl seekArguments () { buffer.position(ARGUMENTS_START); return this; }
    public FrameImpl computeLength () { buffer.putInt(0, buffer.position() - 4); buffer.limit(buffer.position()); return this; }

    /**
     * Sets the position to 0 and the limit to n.
     * @return
     */
    public FrameImpl reset () { buffer.clear(); return this; }
    public FrameImpl limit (int newLimit) { buffer.limit(newLimit); return this; }
    public int limit () { return buffer.limit(); }
    public int position () { return buffer.position(); }
    public int capacity () { return buffer.capacity(); }
    public int remaining () { return buffer.remaining(); }
    public boolean hasRemaining () { return buffer.hasRemaining(); }
    public Frame position (int newPosition) { buffer.position(newPosition); return this; }

    public ByteBuffer getBuffer () { return buffer; }

    public FrameImpl writeTo (WritableByteChannel channel) throws IOException
    {
        channel.write(buffer);
        return this;
    }

    public byte[] toByteArray ()
    {
        final int p = buffer.position();
        final byte[] b = new byte[buffer.limit()];
        buffer.position(0);
        buffer.get(b);
        buffer.position(p);
        return b;
    }

    /**
     *
     */
    @Override
    public String toString ()
    {
        final StringBuilder b = new StringBuilder();
        b.append("Frame").append("[position=").append(buffer.position()).append(",limit=")
                .append(buffer.limit());
        if (buffer.limit() >= 4)
        {
            final int l = getLength();
            b.append(",length=").append(l);
            if (buffer.limit() >= (ARGUMENTS_START - 4))
            {
                // Proceed in spite of invalid length.
                b.append(",sequence=").append(getSequenceNumber()).append(",flags=0x").append(Integer.toString(getFlags(), 16)).append(",object=").append(getObjectNumber()).append(",method=").append(getMethodNumber());
            }
        }
        b.append("]");
        return b.toString();
    }

    /**
     * See {@link ByteBuffer#equals(Object)}.
     */
    @Override
    public boolean equals (Object obj)
    {
        if (!(obj instanceof FrameImpl)) return false;
        final FrameImpl that = (FrameImpl) obj;
        return this.buffer.equals(that.buffer);
    }

    /**
     * See {@link ByteBuffer#hashCode()}
     */
    @Override
    public int hashCode ()
    {
        return this.buffer.hashCode();
    }

}
