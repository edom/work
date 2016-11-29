package com.spacetimecat.web.http.io;

import com.spacetimecat.web.http.syntax.*;
import com.spacetimecat.io.EmptyInputStream;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.nio.BufferOverflowException;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collection;

/**
 * <p>
 *     Parse requests.
 * </p>
 */
public final class HttpInput
{
    private final int maxLineLength;
    private final InputStream input;

    public HttpInput (int maxLineLength, InputStream input)
    {
        this.maxLineLength = maxLineLength;
        this.input = input;
    }

    public Line readLine ()
    {
        final byte[] buffer = new byte[maxLineLength];
        final int length = readLineTo(buffer);
        if (length == 0) { throw new EndOfInputException(); }
        final ByteBuffer byteBuffer = ByteBuffer.wrap(buffer, 0, length);
        final CharBuffer charBuffer = StandardCharsets.US_ASCII.decode(byteBuffer);
        return new Line(charBuffer.toString());
    }

    public HttpRequest readRequest ()
    {
        final HttpRequest x = new HttpRequest();
        x.header = readRequestHeader();
        x.body = new EmptyInputStream();
        return x;
    }

    public HttpResponse readResponse ()
    {
        final HttpResponse x = new HttpResponse();
        x.header = readResponseHeader();
        x.body = new EmptyInputStream();
        return x;
    }

    public RequestHeader readRequestHeader ()
    {
        final RequestHeader result = new RequestHeader();
        final RequestLine requestLine = readRequestLine();
        final Collection<HeaderLine> headerLines = readHeaderLines();
        result.valid = requestLine.valid && headerLines.stream().allMatch(HeaderLine::isValid);
        result.requestLine = requestLine;
        result.headerLines = headerLines;
        return result;
    }

    public ResponseHeader readResponseHeader ()
    {
        final ResponseHeader result = new ResponseHeader();
        final StatusLine statusLine = readStatusLine();
        final Collection<HeaderLine> headerLines = readHeaderLines();
        result.valid = statusLine.valid && headerLines.stream().allMatch(HeaderLine::isValid);
        result.statusLine = statusLine;
        result.headerLines = headerLines;
        return result;
    }

    private Collection<HeaderLine> readHeaderLines ()
    {
        final Collection<HeaderLine> headerLines = new ArrayList<>();
        for (;;)
        {
            final Line line = readLine();
            if (line.isSeparator()) { break; }
            final HeaderLine headerLine = line.asHeaderLine();
            headerLines.add(headerLine);
        }
        return headerLines;
    }

    private StatusLine readStatusLine ()
    {
        final Line line = readLine();
        return line.asStatusLine();
    }

    public RequestLine readRequestLine ()
    {
        final Line line = readLine();
        return line.asRequestLine();
    }

    private int readLineTo (byte[] buffer)
    {
        int count = 0;
        for (;;)
        {
            final int b = read();
            if (b < 0) { break; }
            if (count >= buffer.length) { throw new BufferOverflowException(); }
            buffer[count] = (byte) b;
            ++count;
            if (b == '\n') { break; }
        }
        return count;
    }

    private int read ()
    {
        try
        {
            return input.read();
        }
        catch (IOException e)
        {
            throw new UncheckedIOException(e);
        }
    }
}
