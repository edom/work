package com.spacetimecat.web.http.syntax;

public final class HeaderLine
{
    public boolean valid;
    public String name;
    public String value;
    public String unparsed;

    public static HeaderLine create (String name, String value)
    {
        final HeaderLine result = new HeaderLine();
        result.valid = isValidName(name) && isValidValue(value);
        result.name = name;
        result.value = value;
        result.unparsed = String.format("%s: %s", name, value);
        return result;
    }

    public static HeaderLine parse (String line)
    {
        final HeaderLine result = new HeaderLine();
        final String[] parts = line.split("\\s*:\\s*", 2);
        if (parts.length >= 2)
        {
            result.valid = true;
            result.name = parts[0];
            result.value = parts[1];
            result.unparsed = line;
        }
        return result;
    }

    public static boolean isValidName (String s)
    {
        return s.chars().allMatch(HeaderLine::isValidTokenChar);
    }

    private static String validTokenChars = "\\t !#$%&'*+-.^_`|~0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

    public static boolean isValidTokenChar (int c)
    {
        return validTokenChars.indexOf(c) >= 0;
    }

    public static boolean isValidValue (String s)
    {
        return s.chars().allMatch(HeaderLine::isValidValueChar);
    }

    private static String invalidValueChars = "\r\n";

    private static boolean isValidValueChar (int c)
    {
        return invalidValueChars.indexOf(c) < 0;
    }

    public boolean isValid ()
    {
        return valid;
    }
}
