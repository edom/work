package com.spacetimecat.build.plugin.gradle;

final class Element
{
    private final org.w3c.dom.Element delegate;

    Element (org.w3c.dom.Element delegate)
    {
        this.delegate = delegate;
    }

    Element appendChild (String tag)
    {
        final org.w3c.dom.Element child = delegate.getOwnerDocument().createElement(tag);
        delegate.appendChild(child);
        return new Element(child);
    }

    void setText (String text)
    {
        delegate.setTextContent(text);
    }
}
