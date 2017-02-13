package com.spacetimecat.web.view;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;

public class Template2
{
    protected final Document document;

    public Template2 (Document document)
    {
        this.document = document;
    }

    public static Template2 fromResource (Class<?> base, String path, String baseUri)
    {
        try (InputStream source = base.getResourceAsStream(path))
        {
            return fromInputStream(source, baseUri);
        }
        catch (IOException e)
        {
            throw new UncheckedIOException(e);
        }
    }

    public static Template2 fromInputStream (InputStream source, String baseUri)
    {
        try
        {
            final Document parse = Jsoup.parse(source, "UTF-8", baseUri);
            return new Template2(parse);
        }
        catch (IOException e)
        {
            throw new UncheckedIOException(e);
        }
    }

    public final Document2 instantiate ()
    {
        return new Document2(document.clone());
    }
}
