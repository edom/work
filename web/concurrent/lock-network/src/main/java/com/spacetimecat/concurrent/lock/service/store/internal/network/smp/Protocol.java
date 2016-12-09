package com.spacetimecat.concurrent.lock.service.store.internal.network.smp;

import com.spacetimecat.io.BufferedInputStream2;
import com.spacetimecat.io.BufferedOutputStream2;

import java.io.*;
import java.net.Socket;
import java.nio.BufferOverflowException;
import java.nio.charset.StandardCharsets;

final class Protocol
{
    private static final int LIMIT = 256;

    static final int M_LIST = 1;
    static final int M_ADD = 2;
    static final int M_REMOVE = 3;

    private final DataInput input;
    private final DataOutputStream output;

    Protocol (DataInput input, DataOutputStream output)
    {
        this.input = input;
        this.output = output;
    }

    static Protocol onSocket (Socket socket)
    {
        try
        {
            final int bufferSize = 4096;
            final DataInput input = new DataInputStream(new BufferedInputStream2(socket.getInputStream(), bufferSize));
            final DataOutputStream output = new DataOutputStream(new BufferedOutputStream2(socket.getOutputStream(), bufferSize));
            return new Protocol(input, output);
        }
        catch (IOException e)
        {
            throw new UncheckedIOException(e);
        }
    }

    void flush () throws IOException
    {
        output.flush();
    }

    boolean readBoolean () throws IOException
    {
        return input.readBoolean();
    }

    int readMethod () throws IOException
    {
        return input.readUnsignedShort();
    }

    void writeMethod (int method) throws IOException
    {
        output.writeShort(method);
    }

    void writeBoolean (boolean value) throws IOException
    {
        output.writeBoolean(value);
    }

    int readInt () throws IOException
    {
        return input.readInt();
    }

    void readFully (byte[] bytes) throws IOException
    {
        input.readFully(bytes);
    }

    String readString () throws IOException
    {
        final int length = readInt();
        if (length < 0) { throw new IllegalArgumentException(); }
        if (length > LIMIT) { throw new BufferOverflowException(); }
        final byte[] bytes = new byte[length];
        readFully(bytes);
        return new String(bytes, StandardCharsets.UTF_8);
    }

    void writeString (String string) throws IOException
    {
        final byte[] bytes = string.getBytes(StandardCharsets.UTF_8);
        final int length = bytes.length;
        if (length > LIMIT)
        {
            throw new IllegalArgumentException(String.format("number of bytes in string cannot exceed %s", LIMIT));
        }
        writeInt(length);
        writeBytes(bytes);
    }

    String[] readStringArray () throws IOException
    {
        final int limit = 1048576;
        final int count = readInt();
        if (count < 0) { throw new IllegalArgumentException("count < 0"); }
        if (count > limit) { throw new IllegalArgumentException("count > limit"); }
        final String[] array = new String[count];
        for (int i = 0; i < count; ++i)
        {
            final String string = readString();
            array[i] = string;
        }
        return array;
    }

    void writeStringArray (String... array) throws IOException
    {
        writeInt(array.length);
        for (String string : array)
        {
            writeString(string);
        }
    }

    void writeInt (int value) throws IOException
    {
        output.writeInt(value);
    }

    void writeBytes (byte[] bytes) throws IOException
    {
        output.write(bytes);
    }
}
