package com.spacetimecat.xml.dom;

import java.util.function.Consumer;
import java.util.function.Supplier;

/**
 * <p>
 *     An XML element.
 * </p>
 */
public final class Element extends Node
{
    private final Document d;
    private final org.w3c.dom.Element e;

    Element (Document d, org.w3c.dom.Element e)
    {
        super(e);
        this.d = d;
        this.e = e;
    }

    @Override
    public org.w3c.dom.Element unwrap ()
    {
        return e;
    }

    private Element createElement (String name)
    {
        return d.createElement(name);
    }

    /**
     * <p>
     *     Append a new child element.
     * </p>
     *
     * @param name
     * child element tag name
     *
     * @param sub
     * configure the child element
     *
     * @return
     * this element (not the newly created child)
     */
    public Element element (String name, Consumer<? super Element> sub)
    {
        final Element c = createElement(name);
        e.appendChild(c.unwrap());
        sub.accept(c);
        return this;
    }

    /**
     * <p>
     *     Append a new empty child element.
     * </p>
     *
     * @param name
     * child element tag name
     *
     * @return
     * this element (not the newly created child)
     */
    public Element element (String name)
    {
        return element(name, c -> {});
    }

    /**
     * <p>
     *     Append a new empty child element having a text content.
     * </p>
     *
     * <p>
     *
     * </p>
     *
     * @param name
     * child element tag name
     *
     * @param text
     * child element text content;
     * that child element will contain a text node that contains this text
     *
     * @return
     * this element (not the newly created child)
     */
    public Element element (String name, String text)
    {
        return element(name, c -> c.unwrap().setTextContent(text));
    }

    public Element elementNs (String namespaceUri, String qualifiedName, Consumer<? super Element> sub)
    {
        final Element c = d.createElementNs(namespaceUri, qualifiedName);
        e.appendChild(c.unwrap());
        sub.accept(c);
        return this;
    }

    public Element elementNs (String namespaceUri, String qualifiedName, String text)
    {
        return elementNs(namespaceUri, qualifiedName, c -> c.unwrap().setTextContent(text));
    }

    /**
     * <p>
     *     Set an attribute of this element.
     * </p>
     *
     * @param name
     * attribute name
     *
     * @param value
     * attribute value
     *
     * @return
     * this element
     */
    public Element attribute (String name, String value)
    {
        e.setAttribute(name, value);
        return this;
    }

    /**
     * <p>
     *     Set a qualified attribute of this element.
     * </p>
     *
     * @param namespaceUri
     * example: <code>http://www.w3.org/2001/XMLSchema-instance</code>
     *
     * @param qualifiedName
     * example: <code>xsi:schemaLocation</code>
     * (the prefix is optional)
     *
     * @param value
     * value of the attribute
     *
     * @return
     */
    public Element attributeNs (String namespaceUri, String qualifiedName, String value)
    {
        e.setAttributeNS(namespaceUri, qualifiedName, value);
        return this;
    }

    public Element if_ (Supplier<Boolean> cond, Consumer<? super Element> ifTrue)
    {
        if (cond.get())
        {
            ifTrue.accept(this);
        }
        return this;
    }

    public Element with (Consumer<Element> configure)
    {
        configure.accept(this);
        return this;
    }
}
