package com.spacetimecat.build.files;

import java.io.File;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.FileVisitOption;
import java.nio.file.Paths;
import java.util.List;
import java.util.stream.Collectors;

public final class FilesUnder implements Files
{
    private final String root;

    public FilesUnder (String root)
    {
        this.root = root;
    }

    @Override
    public List<String> get ()
    {
        try
        {
            return java.nio.file.Files
                .walk(Paths.get(root), 8, FileVisitOption.FOLLOW_LINKS)
                .map(Object::toString)
                .collect(Collectors.toList());
        }
        catch (IOException e)
        {
            throw new UncheckedIOException(e);
        }
    }

    public Files directories ()
    {
        return () ->
            FilesUnder.this.get().stream()
                .filter(p -> new File(p).isDirectory())
                .collect(Collectors.toList());
    }

    public Files endingWith (String suffix)
    {
        return () ->
            FilesUnder.this.get().stream()
                .filter(p -> p.endsWith(suffix))
                .collect(Collectors.toList());
    }
}
