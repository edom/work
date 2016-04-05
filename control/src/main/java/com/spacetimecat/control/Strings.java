package com.spacetimecat.control;

import java.util.Collection;

/**
 * {@link String} extension methods.
 *
 * @author erik
 */
public class Strings
{
    /**
     * An example usage of this is for joining classpaths
     * into a colon-separated string to be passed to the -classpath argument.
     *
     * @param separator
     * @param members
     * @return
     */
    public static String join (String separator, Collection<String> members)
    {
        final StringBuilder s = new StringBuilder();
        boolean first = true;
        for (final String e : members)
        {
            if (!first) { s.append(separator); }
            s.append(e);
            first = false;
        }
        return s.toString();
    }
}
