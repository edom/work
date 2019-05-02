package com.spacetimecat.web.http;

import java.util.Locale;
import java.util.Objects;

/**
 * <p>
 *     Media type or media range.
 * </p>
 */
public final class MediaType
{
    private final String unparsed;
    private final String type;
    private final String subtype;

    private MediaType (String unparsed, String type, String subtype)
    {
        this.unparsed = unparsed;
        this.type = type;
        this.subtype = subtype;
    }

    public static MediaType parse (String unparsed)
    {
        final String[] tsAndParams = unparsed.split("\\s*;\\s*");
        final String typeAndSubtype = tsAndParams[0];
        if (typeAndSubtype.equals("*")) { return new MediaType(unparsed, "*", "*"); }
        final String[] ts = typeAndSubtype.split("/");
        if (ts.length != 2) { return null; }
        final String type = ts[0].toLowerCase(Locale.ENGLISH);
        final String subtype = ts[1].toLowerCase(Locale.ENGLISH);
        return new MediaType(unparsed, type, subtype);
    }

    public boolean isSubsetOf (MediaType that)
    {
        return
            (that.type.equals("*") || this.type.equals(that.type))
            &&
            (that.subtype.equals("*") || this.subtype.equals(that.subtype))
            ;
    }

    public String unparsed ()
    {
        return unparsed;
    }

    public String type ()
    {
        return type;
    }

    public String subtype ()
    {
        return subtype;
    }

    public boolean isRoughlyEqualTo (MediaType that)
    {
        return Objects.equals(this.type, that.type)
            && Objects.equals(this.subtype, that.subtype);
    }

    public static MediaType APPLICATION_JSON = parse("application/json");
    public static MediaType TEXT_PLAIN = parse("text/plain");
    public static MediaType TEXT_HTML = parse("text/html");
}
