package com.spacetimecat.web.view;

import org.jsoup.Jsoup;
import org.jsoup.nodes.Document;

import java.io.File;
import java.io.IOException;
import java.io.UncheckedIOException;

/**
 * <p>
 *     For development.
 * </p>
 */
public final class FileTemplate implements Template
{
    private final File file;

    public FileTemplate (String path)
    {
        file = new File(path);
    }

    /**
     * <p>
     *     This always re-opens and re-reads the file
     *     so that you can see the changes
     *     without restarting the application.
     * </p>
     *
     * @return
     * the result of parsing the file
     */
    @Override
    public Document2 instantiate ()
    {
        try
        {
            final Document d = Jsoup.parse(file, "UTF-8");
            return new Document2(d);
        }
        catch (IOException e)
        {
            throw new UncheckedIOException(e);
        }
    }
}
