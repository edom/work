package com.spacetimecat.control;

import java.util.*;

/**
 * Build a HTML fragment.
 *
 * @see #add(Html)
 * @see #render()
 *
 * @author erik
 */
public class Html
{

    private final StringBuilder s = new StringBuilder();
    private final List<Attribute> attributes = new ArrayList<>();

    static class Context
    {
        final Context parent;
        final String tag;
        boolean begin_tag_complete;

        Context (Context parent, String tag)
        {
            this.parent = parent;
            this.tag = tag;
        }
    }

    private Context current = new Context(null, null);
    {
        current.begin_tag_complete = true;
    }

    static class Attribute
    {
        final String name;
        final String value;

        Attribute (String name, String value)
        {
            this.name = name;
            this.value = value;
        }
    }

    public Html begin (String tag)
    {
        flush();
        final Context child = new Context(current, tag);
        current = child;
        s.append('<').append(tag);
        return this;
    }

    public Html attr (String name, String value)
    {
        attributes.add(new Attribute(name, value));
        return this;
    }

    private void flush ()
    {
        if (!current.begin_tag_complete)
        {
            for (final Attribute a : attributes)
            {
                s.append(' ').append(a.name).append("=\"");
                escape(a.value, s);
                s.append('"');
            }
            s.append('>');
            attributes.clear();
            current.begin_tag_complete = true;
        }
    }

    public Html end ()
    {
        flush();
        s.append("</").append(current.tag).append('>');
        current = current.parent;
        return this;
    }

    public Html text (String t)
    {
        flush();
        escape(t, s);
        return this;
    }

    public static String escape (String s)
    {
        final StringBuilder b = new StringBuilder();
        escape(s, b);
        return b.toString();
    }

    /**
     *
     * @param t
     * @param s output argument
     */
    public static void escape (String t, StringBuilder s)
    {
        final int n = t.length();
        for (int i = 0; i < n; ++i)
        {
            final char c = t.charAt(i);
            switch (c)
            {
                case '&': s.append("&amp;"); break;
                case '<': s.append("&lt;"); break;
                case '>': s.append("&gt;"); break;
                case '"': s.append("&quot;"); break;
                default: s.append(c); break;
            }
        }
    }

    public Html text (Object x) { return text(x.toString()); }

    public Html html () { begin("html"); return this; }
    public Html head () { begin("head"); return this; }
    public Html style () { begin("style"); return this; }
    public Html body () { begin("body"); return this; }
    public Html strong () { begin("strong"); return this; }
    public Html strong (String text) { return strong().text(text).end(); }
    public Html p () { begin("p"); return this; }
    public Html div () { begin("div"); return this; }

    public Html h1 () { begin("h1"); return this; }
    public Html h1 (String text) { return h1().text(text).end(); }
    public Html title (String text) { return begin("title").text(text).end(); }

    /**
     * Insert.
     * @param that
     * @return
     */
    public Html add (Html that)
    {
        s.append(that.render());
        return this;
    }

    /**
     * This does not automatically close the tag stack being edited.
     * @return
     */
    public String render ()
    {
        return s.toString();
    }

    public static void main (String[] args)
    {
        final String s = new Html()
                .html()
                    .head()
                        .title("what")
                    .end()
                    .body().attr("class", "narrow").attr("data-what", "\"")
                        .strong("this")
                    .end()
                .end()
                .render();
        System.out.println(s);
    }
}
