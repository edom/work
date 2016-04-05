package com.spacetimecat.control;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * Build a JAR file.
 *
 * @author erik
 */
public class Make_jar
{
    private final String path;
    private final Log log;
    private final List<File> files = new ArrayList<>();

    public Make_jar (Log log, String path)
    {
        this.log = log;
        this.path = path;
    }

    public Make_jar add (Files files)
    {
        this.files.addAll(files.list());
        return this;
    }

    /**
     * The path of the JAR file, as given to the constructor.
     *
     * @return
     */
    public String path ()
    {
        return this.path;
    }

    /**
     * The output of the given javac.
     *
     * This allows easy chaining.
     *
     * @param javac
     * @return
     */
    public Make_jar from (Compile_java javac)
    {
        if (javac.output_dir == null)
        {
            throw new IllegalArgumentException("this relies on javac.output_dir having been set");
        }
        files.add(javac.output_dir);
        return this;
    }

    /**
     * Make the JAR file.
     *
     * @return
     * @throws IOException
     * @throws InterruptedException
     */
    public Make_jar run () throws IOException, InterruptedException
    {
        final List<String> args = new ArrayList<>();
        args.add("jar");
        args.add("cf");
        args.add(path);
        for (final File file : files)
        {
            if (file.isDirectory())
            {
                args.add("-C");
                args.add(file.getAbsolutePath());
                args.add(".");
                continue;
            }
            throw new UnsupportedOperationException("jar currently only supports directories");
        }
        log.info(args.toString());
        Os.exec(args);
        return this;
    }
}
