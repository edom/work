package com.spacetimecat.web.view;

import org.jsoup.select.Elements;

public class Selection2
{
    protected final Elements elements;

    public Selection2 (Elements elements)
    {
        this.elements = elements;
    }

    @Override
    public final String toString ()
    {
        return elements.toString();
    }

    public Element2 first ()
    {
        return new Element2(elements.first());
    }

    public final void setText (String text)
    {
        elements.forEach(element -> element.text(text));
    }

    public final void setValue (String value)
    {
        elements.forEach(element -> element.val(value));
    }

    public final Elements unwrap ()
    {
        return elements;
    }

    public final void remove ()
    {
        elements.remove();
    }

    public final void appendChild (Element2 element)
    {
        elements.forEach(e ->
        {
            e.appendChild(element.unwrap().clone());
        });
    }

    public final void setInnerHtml (String html)
    {
        elements.html(html);
    }
}
