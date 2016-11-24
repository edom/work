package com.spacetimecat.web.example;

import com.spacetimecat.web.servlet.HttpException;
import com.spacetimecat.web.servlet.HttpServlet2;
import com.spacetimecat.web.servlet.Request;
import com.spacetimecat.web.servlet.Response;
import com.spacetimecat.web.view.Template;
import org.jsoup.nodes.Document;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

final class ExampleServlet extends HttpServlet2
{
    private final TodoList todoList = new TodoList();

    private final Template template;

    ExampleServlet (String baseUri)
    {
        this.template = Template.fromResource(ExampleServlet.class, "template.html", baseUri);
    }

    @Override
    protected void serviceHttp (HttpServletRequest request, HttpServletResponse response) throws IOException
    {
        try
        {
            final Document document = template.instantiate();
            final Action action = new Action(document, todoList, new Request(request), new Response(response));
            action.beforeRoute();
            action.route();
            action.afterRoute();
            final String html = action.html();
            response.setStatus(HttpServletResponse.SC_OK);
            response.getWriter().write(html);
            response.setContentType("text/html;charset=UTF-8");
        }
        catch (HttpException e)
        {
            response.sendError(e.getStatus());
        }
    }
}
