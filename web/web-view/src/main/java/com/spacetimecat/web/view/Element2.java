package com.spacetimecat.web.view;

import org.jsoup.nodes.Element;
import org.jsoup.parser.Tag;

public class Element2
{
    protected final Element element;

    public Element2 (Element element)
    {
        this.element = element;
    }

    @Override
    public final String toString ()
    {
        return element.toString();
    }

    public Element2 deepCopy ()
    {
        return new Element2(element.clone());
    }

    public static Element2 create (String tagName)
    {
        return new Element2(new Element(Tag.valueOf(tagName), ""));
    }

    public final void removeClass (String className)
    {
        element.removeClass(className);
    }

    public final Selection2 select (String cssQuery)
    {
        return new Selection2(element.select(cssQuery));
    }

    public final Element unwrap ()
    {
        return element;
    }

    public final void appendChild (Element2 element)
    {
        this.element.appendChild(element.unwrap());
    }

    public final void remove ()
    {
        element.remove();
    }
}
