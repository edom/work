package id.web.edom.rmi.io;

import id.web.edom.rmi.Frame;
import id.web.edom.rmi.InvalidFrameLengthException;

import java.io.EOFException;
import java.io.IOException;

/**
 * <p>Finite-state machine for incrementally reading a frame
 * using non-blocking input.</p>
 *
 * @author erik
 */
public class FrameReader
{
    /**
     * <p>Performs the actual transfer of bytes
     * from a source to the frame.</p>
     *
     * @author erik
     */
    public interface Source
    {
        /**
         * <p>Transfer some bytes from the source to the frame.
         * Should not block.</p>
         *
         * @param buffer the buffer to read to
         *
         * @throws EOFException if the source ended
         * @throws IOException if there was a transput error
         */
        void read (Frame buffer) throws EOFException, IOException;
    }

    private final Source source;
    private final int minLength;
    private final Object lock = new Object();
    private Frame buffer;
    private Frame newBuffer;

    private static final int INIT = 0;
    private static final int READ_LENGTH = 1;
    private static final int READ_THE_REST = 2;

    private int state = INIT;

    private FrameReader (Source source, Frame buffer)
    {
        this.source = source;
        this.buffer = buffer;
        this.newBuffer = buffer;
        this.minLength = buffer.seekArguments().position() - 4;
        buffer.reset();
    }

    /**
     * <p>Schedules the buffer to be replaced.
     * The buffer will be replaced when the next frame begins.</p>
     *
     * @param buffer the new buffer
     */
    public void setBuffer (Frame buffer)
    {
        synchronized (lock)
        {
            this.newBuffer = buffer;
        }
    }

    /**
     * <p>Creates a reader that reads frames from the source into the buffer.</p>
     *
     * @param source usually a closure that uses a non-blocking channel
     * @param buffer will be reused; this frame should be used only by this reader
     */
    public static FrameReader create (Source source, Frame buffer)
    {
        return new FrameReader(source, buffer);
    }

    /**
     * <p>Reads a frame, possibly partially.</p>
     *
     * <p>Also calls {@link FrameImpl#seekArguments()} upon returning non-null
     * so that the returned frame {@linkplain Frame#position() position}
     * is at the beginning of the arguments part of the frame.</p>
     *
     * <p>You should not modify the returned frame.
     * You should not hold a long-lived reference to the frame.</p>
     *
     * @return non-null if and only if a complete frame has been read
     *
     * @throws InvalidFrameLengthException if the frame is smaller than the smallest possible frame
     * @throws EOFException if the channel ends
     * @throws IOException if there is a transput error
     */
    public Frame read () throws InvalidFrameLengthException, EOFException, IOException
    {
        loop: for (;;)
        {
            switch (state)
            {
            case INIT:
                synchronized (lock)
                {
                    buffer = newBuffer;
                }
                buffer.reset().limit(4);
                state = READ_LENGTH;
                continue loop;

            case READ_LENGTH:
                source.read(buffer);
                if (buffer.remaining() == 0)
                {
                    final int length = buffer.getLength();
                    if (length < minLength) { throw new InvalidFrameLengthException(length); }
                    buffer.limit(length + 4);
                    state = READ_THE_REST;
                    continue loop;
                }
                return null;

            case READ_THE_REST:
                source.read(buffer);
                if (buffer.remaining() != 0) { return null; }
                buffer.seekArguments();
                state = INIT;
                return buffer;

            default:
                throw new AssertionError("should not reach here");
            }
        }
    }
}
