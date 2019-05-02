package com.spacetimecat.web.load;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.io.UncheckedIOException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;

/**
 * <p>
 *     For development.
 * </p>
 */
public final class LoadFileBasic implements Load
{
    private final Path base;

    public LoadFileBasic (String base)
    {
        this.base = Paths.get(base);
    }

    @Override
    public Content load (String name)
    {
        final Path path = resolve(name);
        final String type = typeOf(name);
        final byte[] data = read(path);
        return new Content(type, data);
    }

    private Path resolve (String name)
    {
        final Path path = base.resolve(name);
        if (!path.startsWith(base))
        {
            throw new NoSuchFileException(name);
        }
        return path;
    }

    private static byte[] read (Path path)
    {
        try (RandomAccessFile f = new RandomAccessFile(path.toFile(), "r"))
        {
            final long limit = 1048576L;
            final long size = f.length();
            if (size > limit) { throw new FileTooBigException(path.toString()); }
            final byte[] data = new byte[Math.toIntExact(size)];
            f.readFully(data);
            return data;
        }
        catch (FileNotFoundException e)
        {
            throw new NoSuchFileException(e);
        }
        catch (IOException e)
        {
            throw new UncheckedIOException(e);
        }
    }

    private static final Map<String, String> contentType = new HashMap<>();
    static
    {
        contentType.put("css", "text/css;charset=UTF-8");
        contentType.put("js", "application/javascript");
        contentType.put("htm", "text/html;charset=UTF-8");
        contentType.put("html", "text/html;charset=UTF-8");
    }

    static String typeOf (String name)
    {
        final String e = extensionOf(name).toLowerCase();
        final String t = contentType.get(e);
        if (t != null) { return t; }
        throw new UnmappedFileTypeException(name);
    }

    private static String extensionOf (String name)
    {
        final int i = name.lastIndexOf('.');
        if (i < 0) { return ""; }
        return name.substring(i + 1);
    }
}
