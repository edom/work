package com.spacetimecat.web.view;

import org.jsoup.Jsoup;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;

/**
 * <p>
 *     For production.
 * </p>
 */
public final class ResourceTemplate implements Template
{
    private final Document2 document;

    public ResourceTemplate (Class<?> base, String path, String baseUri)
    {
        try (InputStream source = base.getResourceAsStream(path))
        {
            document = new Document2(Jsoup.parse(source, "UTF-8", baseUri));
        }
        catch (IOException e)
        {
            throw new UncheckedIOException(e);
        }
    }

    @Override
    public Document2 instantiate ()
    {
        return document.deepCopy();
    }
}
