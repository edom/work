package com.spacetimecat.control;

import java.io.File;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;

/**
 * Operating system functions.
 *
 * @see Files
 *
 * @author erik
 */
public class Os
{
    private Os () {}
    public static int exec (List<String> args) throws IOException, InterruptedException
    {
        final Process proc =
            new ProcessBuilder(args)
                .inheritIO()
                .start();
        return proc.waitFor();
    }

    public static int exec (String... args) throws IOException, InterruptedException
    {
        return exec(Arrays.asList(args));
    }

    /**
     * <p>Ensure the directory exists and has the given permissions.</p>
     *
     * <p>Security risk: the mode argument is passed as is to chmod
     * (and thus may be used to inject undesired switches).
     * Only pass values you trust as this argument.</p>
     *
     * @param path
     * @param mode first argument to chmod;
     * this can be like "0644", "u=rw,go=r", "u+x";
     * see man chmod for the full details
     */
    public static void mkdir (String path, String mode) throws IOException, InterruptedException
    {
        final File f = new File(path);
        final String abs_path = f.getAbsolutePath();
        if (!f.mkdirs())
        {
            throw new RuntimeException(
                "could not create " + path + "; " +
                    "please make sure that it is not a file " +
                    "and make sure that the nearest directory is writable"
            );
        }
        // Java NIO doesn't have anything to convert octal to Set<PosixFilePermission>.
        final int r = exec("chmod", mode, abs_path);
        if (r != 0)
        {
            throw new IOException("chmod " + mode + " " + abs_path + " exited with code " + r);
        }
    }
}
