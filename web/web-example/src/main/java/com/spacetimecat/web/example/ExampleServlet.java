package com.spacetimecat.web.example;

import com.spacetimecat.web.http.HttpException;
import com.spacetimecat.web.servlet.Entity;
import com.spacetimecat.web.servlet.HttpServlet2;
import com.spacetimecat.web.servlet.Request;
import com.spacetimecat.web.servlet.Response;
import com.spacetimecat.web.view.Document2;
import com.spacetimecat.web.view.Template2;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

final class ExampleServlet extends HttpServlet2
{
    private final TodoList todoList = new TodoList();

    private final Template2 template;

    ExampleServlet (String baseUri)
    {
        this.template = Template2.fromResource(ExampleServlet.class, "template.html", baseUri);
    }


    @Override
    protected void serviceHttp (HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException
    {
        try
        {
            final Document2 document = template.instantiate();
            final Action action = new Action(document.unwrap(), todoList, new Request(request), new Response(response));
            action.beforeRoute();
            action.route();
            action.afterRoute();
            final Entity html = action.html();
            response.setStatus(HttpServletResponse.SC_OK);
            response.setContentType(html.getMediaType());
            html.writeTo(response.getOutputStream());
        }
        catch (HttpException e)
        {
            response.sendError(e.getStatus());
        }
    }
}
