package com.spacetimecat.meta.rt.java;

import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.nio.BufferOverflowException;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;

public final class Meta_runtime {

    private static ByteBuffer slurp (InputStream stream, int limit) throws IOException {
        byte[] buffer = new byte[limit];
        int total = 0;
        for (;;) {
            final int remain = limit - total;
            final int count = stream.read(buffer, total, remain);
            if (count < 0) {
                return ByteBuffer.wrap(buffer, 0, total);
            }
            if (remain <= 0) {
                throw new BufferOverflowException();
            }
            total += count;
        }
    }

    public static String read_string (URL url, int max_byte_count) throws IOException {
        try (InputStream stream = url.openStream()) {
            final ByteBuffer buffer = slurp(stream, max_byte_count);
            return StandardCharsets.UTF_8.decode(buffer).toString();
        } catch (BufferOverflowException e) {
            throw new IOException("resource size exceeds " + max_byte_count + " byte limit: " + url);
        }
    }

    /**
     * <p>
     *     This is {@link Class#getResource(String)}
     *     that throws an {@link IOException} instead of returning null
     *     when the resource can't be found.
     * </p>
     */
    public static URL get_resource (Class<?> cls, String path) throws IOException {
        final URL url = cls.getResource(path);
        if (url == null) { throw new IOException("cannot find resource: " + path); }
        return url;
    }

    public static String get_resource_as_string (Class<?> cls, String path, int max_byte_count) throws IOException {
        return read_string(get_resource(cls, path), max_byte_count);
    }

    private Meta_runtime () {}

}
