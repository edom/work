package com.spacetimecat.web.servlet.logic;

import javax.servlet.ServletException;
import javax.servlet.ServletOutputStream;
import javax.servlet.WriteListener;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpServletResponseWrapper;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.zip.GZIPOutputStream;

public final class Gzip implements Logic
{
    private final Logic inner;

    public Gzip (Logic inner)
    {
        this.inner = inner;
    }

    @Override
    public boolean handle (HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException
    {
        final Output output = new Output(response.getOutputStream());
        final boolean handled = inner.handle(request, new Wrapper(response, output));
        if (handled)
        {
            response.setHeader("Content-Encoding", "gzip");
            output.finish();
        }
        return handled;
    }

    private static final class Wrapper extends HttpServletResponseWrapper
    {
        private final Output output;

        private Wrapper (HttpServletResponse response, Output output)
        {
            super(response);
            this.output = output;
        }

        @Override
        public ServletOutputStream getOutputStream () throws IOException
        {
            return output;
        }

        @Override
        public PrintWriter getWriter () throws IOException
        {
            return new PrintWriter(getOutputStream());
        }
    }

    private static final class Output extends ServletOutputStream
    {
        private final ServletOutputStream delegate;
        private final GZIPOutputStream gzip;

        private Output (ServletOutputStream delegate) throws IOException
        {
            this.delegate = delegate;
            this.gzip = new GZIPOutputStream(delegate);
        }

        private void finish () throws IOException
        {
            gzip.finish();
        }

        @Override
        public boolean isReady ()
        {
            return delegate.isReady();
        }

        @Override
        public void setWriteListener (WriteListener writeListener)
        {
            delegate.setWriteListener(writeListener);
        }

        @Override
        public void write (int b) throws IOException
        {
            gzip.write(b);
        }
    }
}
