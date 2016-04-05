package com.spacetimecat.control;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

/**
 * Compile Java source files into class files.
 *
 * @author erik
 */
public class Compile_java
{

    private final Log log;
    private final List<File> files = new ArrayList<>();
    private final List<String> classpath = new ArrayList<>();
    File output_dir;

    public Compile_java (Log log)
    {
        this.log = log;
    }

    /**
     * You can only set this once.
     *
     * @param path should not be null
     * @return
     */
    public Compile_java output_dir (String path)
    {
        if (output_dir != null)
        {
            throw new IllegalStateException("output_dir is already set");
        }
        output_dir = new File(path);
        return this;
    }

    public Compile_java add (File file)
    {
        this.files.add(file);
        return this;
    }

    public Compile_java add (Collection<File> files)
    {
        this.files.addAll(files);
        return this;
    }

    public Compile_java add (String... paths)
    {
        for (final String path : paths)
        {
            add(new File(path));
        }
        return this;
    }

    public Compile_java add (Files files)
    {
        return add(files.list());
    }

    /**
     * Add all Java source files under 'src' directory.
     *
     * Set output directory to 'out'.
     *
     * @return
     */
    public Compile_java conventional ()
    {
        return
            add(Files.under("src").having_extension("java"))
            .output_dir("out");
    }

    /**
     * Append (do not replace) the given paths to the classpath.
     *
     * @param paths
     * @return
     */
    public Compile_java classpath (String... paths)
    {
        classpath.addAll(Arrays.asList(paths));
        return this;
    }

    public Compile_java classpath (Files files)
    {
        for (final File f : files.list())
        {
            classpath.add(f.getAbsolutePath());
        }
        return this;
    }

    /**
     * Compile the sources.
     *
     * Run the compiler with this object as the configuration.
     *
     * @return
     * @throws IOException
     * @throws InterruptedException
     */
    public Compile_java run () throws IOException, InterruptedException
    {
        if (output_dir == null)
        {
            throw new IllegalStateException("must set output_dir before run");
        }
        output_dir.mkdirs();
        final List<String> args = new ArrayList<>();
        args.add("javac");
        args.add("-d");
        args.add(output_dir.getAbsolutePath());
        if (!classpath.isEmpty())
        {
            args.add("-classpath");
            args.add(Strings.join(":", classpath));
        }
        for (final File file : files)
        {
            args.add(file.getAbsolutePath());
        }
        log.info(args.toString());
        final int exit_code = Os.exec(args);
        if (exit_code != 0)
        {
            log.error("java compiler exited with code " + exit_code);
        }
        return this;
    }

}
