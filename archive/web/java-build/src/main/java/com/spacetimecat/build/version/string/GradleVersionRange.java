package com.spacetimecat.build.version.string;

import com.spacetimecat.build.dependency.Version;
import com.spacetimecat.build.math.bound.LowerBound;
import com.spacetimecat.build.math.bound.UpperBound;
import com.spacetimecat.build.math.extended.Extended;
import com.spacetimecat.build.math.range.Range;

public final class GradleVersionRange
{
    private final Range<Extended<Version>> range;

    public GradleVersionRange (Range<Extended<Version>> range)
    {
        this.range = range;
    }

    @Override
    public String toString ()
    {
        final StringBuilder s = new StringBuilder();

        final LowerBound<Extended<Version>> lowerBound = range.lowerBound();
        final UpperBound<Extended<Version>> upperBound = range.upperBound();
        final char startBracket = lowerBound.type().isOpen() ? '(' : '[';
        final char endBracket = upperBound.type().isOpen() ? ')' : ']';
        final String lowerValue = lowerBound.value().isFinite() ? lowerBound.value().toString() : "";
        final String upperValue = upperBound.value().isFinite() ? upperBound.value().toString() : "";
        s.append(startBracket);
        s.append(lowerValue);
        s.append(',');
        s.append(upperValue);
        s.append(endBracket);
        return s.toString();
    }
}
