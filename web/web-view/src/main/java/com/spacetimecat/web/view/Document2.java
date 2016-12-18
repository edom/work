package com.spacetimecat.web.view;

import org.jsoup.nodes.Document;

public class Document2
{
    protected final Document document;

    public Document2 (Document document)
    {
        this.document = document;
    }

    @Override
    public final String toString ()
    {
        return document.toString();
    }

    public final Selection2 select (String cssQuery)
    {
        return new Selection2(document.select(cssQuery));
    }

    public final Document unwrap ()
    {
        return document;
    }
}
