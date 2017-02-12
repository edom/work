package com.spacetimecat.build.java;

import com.spacetimecat.build.files.Paths;

import java.io.File;

public class JavaCompiler
{
    private final String path;

    public JavaCompiler (String path)
    {
        this.path = path;
    }

    public final CompileJava compile (File outputDir, Paths sources)
    {
        return new CompileJava(path, outputDir, sources);
    }
}
