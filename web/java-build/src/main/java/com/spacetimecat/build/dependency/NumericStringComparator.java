package com.spacetimecat.build.dependency;

import java.io.Serializable;
import java.util.Comparator;

/**
 * <p>
 *     Compare as number, or compare lexicographically if fail.
 * </p>
 * <p>
 *     This depends on the fact that the hyphen precedes the dot in ASCII.
 * </p>
 */
public final class NumericStringComparator implements Comparator<String>, Serializable
{
    @Override
    public int compare (String a, String b)
    {
        try
        {
            return Integer.parseInt(a) - Integer.parseInt(b);
        }
        catch (NumberFormatException e)
        {
            return a.compareTo(b);
        }
    }
}
