package com.spacetimecat.control;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

/**
 * <p>Specify list of files. Use the static methods to get instances.</p>
 *
 * @see Os
 *
 * @author erik
 */
public class Files
{
    private final List<File> result = new ArrayList<>();

    private Files () {}

    /**
     * Filter this file list so that it contains only the paths that has the given extension.
     *
     * This is case-sensitive.
     *
     * Do not put a leading period in 'ext'.
     *
     * @param ext
     * @return
     */
    public Files having_extension (String ext)
    {
        final String suffix = "." + ext;
        final Iterator<File> iterator = result.iterator();
        while (iterator.hasNext())
        {
            final File file = iterator.next();
            final String name = file.getName();
            if (!name.endsWith(suffix))
            {
                iterator.remove();
            }
        }
        return this;
    }

    /**
     * All files recursively under the root.
     *
     * If the root is a file, this returns a collection containing one file, that is the root itself.
     *
     * If the root is not a file, or there is an IO error,
     * this returns an empty collection. This never returns null.
     *
     * There will be no directory in the returned collection.
     *
     * In the future, we may make this method throw {@link java.io.IOException}
     * if there is an IO error.
     *
     * @param root
     * @return a collection of files; never null
     */
    public static Files under (File root)
    {
        final Files result = new Files();
        result.in_(0, root);
        return result;
    }

    public static Files under (String root)
    {
        return under(new File(root));
    }

    public List<File> list ()
    {
        return Collections.unmodifiableList(result);
    }

    private void in_ (int depth, File root)
    {
        if (depth >= 64)
        {
            throw new RuntimeException("recursion too deep, probably due to cyclic symbolic link");
        }
        final String name = root.getName();
        if (name.equals(".") || name.equals(".."))
        {
            return;
        }
        if (root.isFile())
        {
            result.add(root);
            return;
        }
        // Why does listFiles return null instead of throwing IO exception when there is an IO error?
        final File[] children = root.listFiles();
        if (children == null)
        {
            // root is not a directory or there is an IO error.
            return;
        }
        for (final File child : children)
        {
            in_(depth + 1, child);
        }
    }
}
