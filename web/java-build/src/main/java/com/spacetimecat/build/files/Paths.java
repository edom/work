package com.spacetimecat.build.files;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.file.FileVisitOption;
import java.nio.file.Path;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public final class Paths
{
    private final Stream<Path> inner;

    protected Paths (Stream<Path> inner)
    {
        this.inner = inner;
    }

    public static Paths under (String root)
    {
        try
        {
            return new Paths(java.nio.file.Files
                .walk(java.nio.file.Paths.get(root), 16, FileVisitOption.FOLLOW_LINKS)
            );
        }
        catch (IOException e)
        {
            throw new UncheckedIOException(e);
        }
    }

    public Paths filter (Predicate<? super Path> predicate)
    {
        return new Paths(inner.filter(predicate));
    }

    public Paths whichAreDirectories ()
    {
        return filter(p -> p.toFile().isDirectory());
    }

    public Paths containing (String subpath)
    {
        return filter(p -> p.resolve(subpath).toFile().exists());
    }

    public Paths whoseNameEndsWith (String suffix)
    {
        return filter(p -> p.toString().endsWith(suffix));
    }

    public Paths whichLooksLikeMavenProject ()
    {
        return whichAreDirectories().containing("src/main/java");
    }

    public List<Path> collect ()
    {
        return inner.collect(Collectors.toList());
    }

    public List<String> collectString ()
    {
        return string().collect(Collectors.toList());
    }

    public Stream<String> string ()
    {
        return map(Object::toString);
    }

    public <T> Stream<T> map (Function<? super Path, ? extends T> function)
    {
        return inner.map(function);
    }

    @Override
    public String toString ()
    {
        return collectString().toString();
    }

    public void forEach (Consumer<? super Path> action)
    {
        inner.forEach(action);
    }
}
