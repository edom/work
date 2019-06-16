package com.spacetimecat.java.prolog2.parser;

import java.io.IOException;
import java.io.Reader;
import java.io.UncheckedIOException;
import java.nio.IntBuffer;

/**
 * A stream of characters, but this stream can be "unread".
 */
final class Stream {

    static final int EOF = -1;

    final IntBuffer pending = IntBuffer.allocate(4096);
    final Reader in;
    int begin = 0;
    int next = 0;

    Stream (Reader in) {
        this.in = in;
    }

    int getc () {
        if (!(begin <= next)) {
            throw new IllegalStateException("attempting to backtrack too far to data that has been discarded by commit");
        }
        if (next < begin + pending.position()) {
            return pending.get(next - begin);
        }
        try {
            int c = in.read();
            if (c == EOF) {
                return c;
            }
            pending.put(c);
            ++next;
            return c;
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    void backtrack_to (int position) {
        this.next = position;
    }

    void commit () {
        pending.position(pending.position() + next - begin);
        begin = next;
    }

}
