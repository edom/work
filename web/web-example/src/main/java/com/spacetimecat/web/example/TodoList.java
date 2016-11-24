package com.spacetimecat.web.example;

import org.jsoup.nodes.Element;

import java.util.ArrayList;
import java.util.List;

final class TodoList
{
    private final Object lock = new Object();

    private final List<String> list = new ArrayList<>();

    void add (String item)
    {
        synchronized (lock)
        {
            list.add(item);
        }
    }

    void renderInto (Element root)
    {
        root.empty();
        synchronized (list)
        {
            list.forEach(item ->
            {
                final Element li = root.appendElement("li");
                li.text(item);
            });
        }
    }
}
