package com.spacetimecat.web.load;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.io.UncheckedIOException;
import java.nio.file.Path;
import java.nio.file.Paths;

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
        final String type = typeOf(path);
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

    private static String typeOf (Path path)
    {
        final String name = path.getFileName().toString().toLowerCase();
        if (name.endsWith(".css")) { return "text/css;charset=UTF-8"; }
        if (name.endsWith(".js")) { return "application/javascript"; }
        throw new UnmappedFileTypeException(path.toString());
    }
}
