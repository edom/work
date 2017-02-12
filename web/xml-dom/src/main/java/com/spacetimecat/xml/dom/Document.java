package com.spacetimecat.xml.dom;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import java.io.ByteArrayOutputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.charset.StandardCharsets;
import java.util.function.Consumer;

/**
 * <p>
 *     An XML document.
 * </p>
 */
public final class Document extends Node
{
    private final org.w3c.dom.Document d;

    private Document (org.w3c.dom.Document d)
    {
        super(d);
        this.d = d;
    }

    /**
     * <p>
     *     Create a new empty document.
     * </p>
     *
     * @return
     * a new empty document
     */
    public static Document newEmpty ()
    {
        try
        {
            final DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            final DocumentBuilder builder = factory.newDocumentBuilder();
            final org.w3c.dom.Document d = builder.newDocument();
            return new Document(d);
        }
        catch (ParserConfigurationException e)
        {
            throw new RuntimeException(e);
        }
    }

    @Override
    public org.w3c.dom.Document unwrap ()
    {
        return d;
    }

    final Element createElement (String name)
    {
        return new Element(this, d.createElement(name));
    }

    final Element createElementNs (String namespaceUri, String qualifiedName)
    {
        return new Element(this, d.createElementNS(namespaceUri, qualifiedName));
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
     * this document (not the newly created child)
     */
    public Document element (String name, Consumer<? super Element> sub)
    {
        final Element e = createElement(name);
        d.appendChild(e.unwrap());
        sub.accept(e);
        return this;
    }

    public Document elementNs (String namespaceUri, String qualifiedName, Consumer<? super Element> sub)
    {
        final Element e = createElementNs(namespaceUri, qualifiedName);
        d.appendChild(e.unwrap());
        sub.accept(e);
        return this;
    }

    /**
     * <p>
     *     Render this document in UTF-8 encoding.
     * </p>
     *
     * @return
     * bytes that encode a string in UTF-8
     */
    public byte[] toByteArray ()
    {
        try
        {
            final ByteArrayOutputStream o = new ByteArrayOutputStream();
            final Transformer t = TransformerFactory.newInstance().newTransformer();
            t.setOutputProperty(OutputKeys.INDENT, "yes");
            t.setOutputProperty(OutputKeys.ENCODING, "UTF-8");
            t.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "4");
            t.transform(new DOMSource(d), new StreamResult(o));
            return o.toByteArray();
        }
        catch (TransformerException e)
        {
            throw new RuntimeException(e);
        }
    }

    /**
     * <p>
     *     Wrap the return value of {@link #toByteArray()} in a {@link String}.
     * </p>
     *
     * @return
     * the return value of {@link #toByteArray()} as a {@link String}
     */
    @Override
    public String toString ()
    {
        return new String(toByteArray(), StandardCharsets.UTF_8);
    }

    public void writeToFile (String path)
    {
        try (FileOutputStream s = new FileOutputStream(path))
        {
            s.write(toByteArray());
        }
        catch (IOException e)
        {
            throw new UncheckedIOException(e);
        }
    }
}
