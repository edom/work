package com.spacetimecat.web.example;

import com.spacetimecat.web.servlet.NotFoundException;
import com.spacetimecat.web.servlet.Param;
import com.spacetimecat.web.servlet.Request;
import com.spacetimecat.web.servlet.Response;
import com.spacetimecat.web.view.Selection2;
import org.jsoup.nodes.Document;

import java.util.Locale;

final class Action
{
    private final Document document;
    private final Request request;
    private final TodoList todoList;
    private final Response response;

    Action (Document document, TodoList todoList, Request request, Response response)
    {
        this.document = document;
        this.request = request;
        this.todoList = todoList;
        this.response = response;
    }

    void beforeRoute ()
    {
        initState("name");

        final Param<String> name = request.getParameter("name");
        if (name.exists())
        {
            new Selection2(document.select(".greeting .name")).setText(name.optional());
        }
        else
        {
            document.select(".greeting").remove();
        }
    }

    void route ()
    {
        final String method = request.getMethod();
        final String path = request.getPathInfo();
        if (method.equals("GET"))
        {
            if (path.equals("/")) { index(); return; }
            throw new NotFoundException();
        }
        if (method.equals("POST"))
        {
            if (path.equals("/greet")) { greet(); return; }
            if (path.equals("/todo/add")) { addTodo(); return; }
            throw new NotFoundException();
        }
        throw new NotFoundException();
    }

    void afterRoute ()
    {
        todoList.renderInto(document.select(".todo ul").first());
    }

    private void initState (String name)
    {
        request.getParameter(name).ifExistsDo(value ->
        {
            final String cssQuery = String.format((Locale) null, "input[name=%s]", name);
            new Selection2(document.select(cssQuery)).setValue(value);
        });
    }

    private void index ()
    {
    }

    private void greet ()
    {
        request.getParameter("name").required();
    }

    private void addTodo ()
    {
        final String item = request.getParameter("item").required();
        todoList.add(item);
    }

    String html ()
    {
        return document.html();
    }
}
