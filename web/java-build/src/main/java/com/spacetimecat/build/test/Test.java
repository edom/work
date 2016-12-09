package com.spacetimecat.build.test;

import com.spacetimecat.build.dependency.NumericStringComparator;
import com.spacetimecat.build.dependency.Version;
import com.spacetimecat.build.math.extended.Extended;
import com.spacetimecat.build.math.range.Range;
import com.spacetimecat.build.math.vector.PrefixComparator;
import com.spacetimecat.build.version.string.GradleVersionRangeString;
import com.spacetimecat.build.version.string.VersionString;
import com.spacetimecat.java.lang.compare.NaturalComparator;

final class Test
{
    public static void main (String[] args)
    {
        final GradleVersionRangeString a = new GradleVersionRangeString("[1.2.3,2.3.4]");
        final GradleVersionRangeString b = new GradleVersionRangeString("]1.2.3,2.3.4[");
        final GradleVersionRangeString c = new GradleVersionRangeString("(,)");
        final Range<Extended<Version>> x = a.parse();
        final Range<Extended<Version>> y = b.parse();
        final Range<Extended<Version>> z = c.parse();
        final Version u = new VersionString("1.2-SNAPSHOT").parse();
        final Version v = new VersionString("1.2.0-SNAPSHOT").parse();
        final int j = Version.Comparator(new PrefixComparator<String>(new NumericStringComparator())).compare(u, v);
        final int k = Version.Comparator(new PrefixComparator<String>(new NaturalComparator<String>())).compare(u, v);
        System.out.println(j);
        System.out.println(k);
        System.out.println(x);
        System.out.println(y);
        System.out.println(z);
    }
}
