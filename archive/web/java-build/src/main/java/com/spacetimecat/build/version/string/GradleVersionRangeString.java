package com.spacetimecat.build.version.string;

import com.spacetimecat.build.dependency.Version;
import com.spacetimecat.build.math.bound.LowerBound;
import com.spacetimecat.build.math.bound.Type;
import com.spacetimecat.build.math.bound.UpperBound;
import com.spacetimecat.build.math.extended.Extended;
import com.spacetimecat.build.math.extended.NegativeInfinity;
import com.spacetimecat.build.math.extended.Normal;
import com.spacetimecat.build.math.extended.PositiveInfinity;
import com.spacetimecat.build.math.range.Range;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * <p>
 *     {@link String} representing a version range such as {@code [1.2.3,2.3.4]}.
 * </p>
 * <p>
 *     A version string {@code 1.2.3} is treated as a range containing exactly one element,
 *     that is {@code [1.2.3,1.2.3]}.
 * </p>
 * <p>
 *     It seems that Gradle accepts Bourbaki-style interval notations in the build.gradle file,
 *     but internally normalizes everything into traditional-style notation.
 * </p>
 */
public class GradleVersionRangeString
{
    private final String string;

    public GradleVersionRangeString (String string)
    {
        this.string = string;
    }

    public Range<Extended<Version>> parse ()
    {
        final Pattern rangePattern = Pattern.compile("^(\\[|\\]|\\()(.*?),(.*?)(\\[|\\]|\\))$");
        final Matcher matcher = rangePattern.matcher(string);
        final boolean isRange = matcher.matches();
        if (isRange)
        {
            final String lowerBoundType = matcher.group(1);
            final String lowerBoundValue = matcher.group(2);
            final String upperBoundValue = matcher.group(3);
            final String upperBoundType = matcher.group(4);

            final Version lower = new VersionString(lowerBoundValue).parse();
            final Version upper = new VersionString(upperBoundValue).parse();

            final Type lowerType;
            final Type upperType;

            switch (lowerBoundType)
            {
                case "[":
                    lowerType = Type.closed();
                    break;
                case "(":
                case "]":
                    lowerType = Type.open();
                    break;
                default:
                    throw new AssertionError(lowerBoundType);
            }

            switch (upperBoundType)
            {
                case ")":
                case "[":
                    upperType = Type.open();
                    break;
                case "]":
                    upperType = Type.closed();
                    break;
                default:
                    throw new AssertionError(upperBoundType);
            }

            final Extended<Version> lowerValue = lower.isEmpty() ? new NegativeInfinity<>() : new Normal<>(lower);
            final Extended<Version> upperValue = upper.isEmpty() ? new PositiveInfinity<>() : new Normal<>(upper);;

            final LowerBound<Extended<Version>> lowerBound = new LowerBound<>(lowerType, lowerValue);
            final UpperBound<Extended<Version>> upperBound = new UpperBound<>(upperType, upperValue);

            return new Range<>(lowerBound, upperBound);
        }
        else
        {
            final Version version = new VersionString(string).parse();
            return new Range<>(
                new LowerBound<>(Type.closed(), new Normal<>(version))
                , new UpperBound<>(Type.closed(), new Normal<>(version))
            );
        }
    }

    @Override
    public String toString ()
    {
        return string;
    }
}
