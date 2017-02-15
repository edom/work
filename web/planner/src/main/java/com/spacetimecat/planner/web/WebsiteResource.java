package com.spacetimecat.planner.web;

import com.spacetimecat.web.load.Content;
import com.spacetimecat.web.load.Load;
import com.spacetimecat.web.view.Template;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Response;

@Produces("text/html;charset=UTF-8")
@Path("/")
public final class WebsiteResource
{
    private final Load loadStatic;
    private final Template template;

    public WebsiteResource (Load loadStatic, Template template)
    {
        this.loadStatic = loadStatic;
        this.template = template;
    }

    @GET
    @Path("")
    public String hello ()
    {
        return template.instantiate().toString();
    }

    @GET
    @Path("statics/{path}")
    public Response foo (@PathParam("path") String path)
    {
        final Content content = loadStatic.load(path);
        return Response.ok(content.getData(), content.getType()).build();
    }
}
