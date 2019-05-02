package com.spacetimecat.build.dependency;

import com.spacetimecat.build.math.bound.LowerBound;
import com.spacetimecat.build.math.bound.Type;
import com.spacetimecat.build.math.bound.UpperBound;
import com.spacetimecat.build.math.extended.Extended;
import com.spacetimecat.build.math.extended.ExtendedComparator;
import com.spacetimecat.build.math.extended.NegativeInfinity;
import com.spacetimecat.build.math.extended.PositiveInfinity;
import com.spacetimecat.build.math.range.Intersection;
import com.spacetimecat.build.math.range.Range;
import com.spacetimecat.build.math.vector.PrefixComparator;

import java.util.Comparator;
import java.util.HashMap;
import java.util.Map;

public final class ConstraintAccumulator
{
    private static final Comparator<Extended<Version>> comparator =
        new ExtendedComparator<>(
            Version.Comparator(
                new PrefixComparator<>(new NumericStringComparator())
            )
        );

    private final Map<String, Range<Extended<Version>>> resolutions = new HashMap<>();

    public Range<Extended<Version>> add (String name, Range<Extended<Version>> constraint)
    {
        return resolutions.compute(name, (key, accumulator) ->
        {
            if (accumulator == null)
            {
                accumulator = entireVersionRange();
            }
            return new Intersection<>(comparator, accumulator, constraint).compute();
        });
    }

    private static Range<Extended<Version>> entireVersionRange ()
    {
        return new Range<Extended<Version>>(
            new LowerBound<Extended<Version>>(Type.open(), new NegativeInfinity<Version>())
            , new UpperBound<Extended<Version>>(Type.open(), new PositiveInfinity<Version>())
        );
    }
}
