package com.spacetimecat.control;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * Run a Java program.
 *
 * @author erik
 */
public class Run_java
{
    private final Log log;

    private final List<String> classpath = new ArrayList<>();
    private String main;

    public Run_java (Log log)
    {
        this.log = log;
    }

    public Run_java run () throws IOException, InterruptedException
    {
        if (main == null)
        {
            throw new IllegalStateException("main class has not been set");
        }
        final List<String> args = new ArrayList<>();
        args.add("java");
        if (!classpath.isEmpty())
        {
            args.add("-classpath");
            // This assumes that classpath does not contain a path having a colon.
            args.add(Strings.join(":", classpath));
        }
        args.add(main);
        log.info(args.toString());
        Os.exec(args);
        return this;
    }

    public Run_java classpath (String path)
    {
        this.classpath.add(path);
        return this;
    }

    public Run_java main (String class_name)
    {
        this.main = class_name;
        return this;
    }
}
