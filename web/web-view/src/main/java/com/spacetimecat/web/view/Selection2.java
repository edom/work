package com.spacetimecat.web.view;

import org.jsoup.select.Elements;

public class Selection2
{
    private final Elements selection;

    public Selection2 (Elements selection)
    {
        this.selection = selection;
    }

    public void setText (String text)
    {
        selection.forEach(element -> element.text(text));
    }

    public void setValue (String value)
    {
        selection.forEach(element -> element.val(value));
    }
}
