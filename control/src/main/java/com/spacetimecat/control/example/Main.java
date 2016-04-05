package com.spacetimecat.control.example;

import com.spacetimecat.control.*;

import java.io.IOException;

/**
 * This is an example control file.
 *
 * @author erik
 */
public class Main
{

    public static void main (String[] args) throws IOException, InterruptedException
    {
        final Log log = new Log.Std();
        final Compile_java javac = new Compile_java(log)
            .add(Files.under("src/main/java").having_extension("java"))
            .classpath(Files.under("lib").having_extension("jar"))
            .output_dir("tmp-out")
            .run();
        final Make_jar jar = new Make_jar(log, "tmp-out.jar")
            .from(javac)
            .run();
        new Run_java(log)
            .classpath(jar.path())
            .main("com.spacetimecat.control.example.Main$Hello")
            .run();
    }

    /**
     * @author erik
     */
    public static class Hello
    {
        public static void main (String[] args)
        {
            System.out.println("hello");
        }
    }
}
