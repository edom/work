package com.spacetimecat.io;

import java.nio.ByteBuffer;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Arrays;

public final class Result
{
    private final ByteBuffer delegate;

    Result (ByteBuffer delegate)
    {
        this.delegate = delegate;
    }

    /**
     * <p>
     *     Do not mutate the returned object.
     * </p>
     * @return an instance
     */
    public ByteBuffer toByteBuffer ()
    {
        return delegate;
    }

    /**
     * <p>
     *     Do not mutate the returned object.
     * </p>
     * @return an instance
     */
    public byte[] toByteArray ()
    {
        return Arrays.copyOf(delegate.array(), delegate.limit());
    }

    public String toString (Charset charset)
    {
        return charset.decode(delegate).toString();
    }

    /**
     * <p>
     *     Decode the bytes as UTF-8 string.
     * </p>
     * @return a string
     */
    @Override
    public String toString ()
    {
        return toString(StandardCharsets.UTF_8);
    }
}
