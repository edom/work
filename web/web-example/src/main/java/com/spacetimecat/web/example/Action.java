package com.spacetimecat.web.example;

import com.spacetimecat.web.http.NotFoundException;
import com.spacetimecat.web.http.MediaType;
import com.spacetimecat.web.http.param.Param;
import com.spacetimecat.web.servlet.*;
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

        final Param<String> name = request.parameters().get("name");
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
        final MediaType mediaType = request.negotiation().offer(
            MediaType.TEXT_HTML
            , MediaType.APPLICATION_JSON
        ).strict();
        if (mediaType.isRoughlyEqualTo(MediaType.TEXT_HTML))
        {
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
        throw new NotFoundException();
    }

    void afterRoute ()
    {
        todoList.renderInto(document.select(".todo ul").first());
    }

    private void initState (String name)
    {
        request.parameters().get(name).ifExistsDo(value ->
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
        request.parameters().require("name");
    }

    private void addTodo ()
    {
        final String item = request.parameters().get("item").required();
        todoList.add(item);
    }

    Entity html ()
    {
        return DefaultEntity.textHtml(document.html());
    }
}
