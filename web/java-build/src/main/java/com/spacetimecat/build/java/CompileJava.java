package com.spacetimecat.build.java;

import com.spacetimecat.build.files.Paths;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;

public final class CompileJava implements Callable<Void>
{
    private final String javac;
    private final File outputDir;
    private final Paths sources;

    CompileJava (String javac, File outputDir, Paths sources)
    {
        this.javac = javac;
        this.outputDir = outputDir;
        this.sources = sources;
    }

    @Override
    public Void call () throws IOException, InterruptedException
    {
        final List<String> files = sources.collectString();
        final List<String> command = new ArrayList<>(3 + files.size());
        command.add(javac);
        command.add("-d");
        command.add(outputDir.getAbsolutePath());
        command.addAll(files);
        final Process p = new ProcessBuilder(command).start();
        final int code = p.waitFor();
        if (code != 0) { throw new ExitException(code, command); }
        return null;
    }
}
