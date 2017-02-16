package com.spacetimecat.planner.web;

import com.spacetimecat.planner.ManHour;
import com.spacetimecat.planner.Milestone;
import com.spacetimecat.web.load.Content;
import com.spacetimecat.web.load.Load;
import com.spacetimecat.web.view.Document2;
import com.spacetimecat.web.view.Element2;
import com.spacetimecat.web.view.Selection2;
import com.spacetimecat.web.view.Template;
import org.hibernate.validator.constraints.NotEmpty;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;
import java.net.URI;
import java.util.ArrayList;
import java.util.List;

@Produces("text/html;charset=UTF-8")
@Path("/")
public final class WebsiteResource
{
    private final Load loadStatic;
    private final Template template;
    private final List<Milestone> milestones = new ArrayList<>();

    public WebsiteResource (Load loadStatic, Template template)
    {
        this.loadStatic = loadStatic;
        this.template = template;
    }

    @GET
    public String hello ()
    {
        final List<Milestone> milestones;
        synchronized (this.milestones)
        {
            milestones = new ArrayList<>(this.milestones);
        }
        final int count = milestones.size();
        final Document2 d = template.instantiate();
        new Localize(d).number("milestoneCountMessage", count);
        d.select(".milestoneCount").setText(count);
        final Selection2 ul = d.select("ul");
        milestones.forEach(m ->
        {
            final Element2 li = Element2.create("li");
            li.appendText(m.getDescription() + " (" + m.getEffort() + ")");
            ul.appendChild(li);
        });
        return d.toString();
    }

    @POST
    @Path("milestone-add")
    public Response milestoneAdd
    (
        @FormParam("description")
        @NotEmpty
        String description
        ,
        @FormParam("effort")
        int effort
    )
    {
        final Milestone m = new Milestone(description, new ManHour(effort));
        synchronized (milestones)
        {
            milestones.add(m);
        }
        return Response.seeOther(URI.create("/")).build();
    }

    @GET
    @Path("statics/{path}")
    public Response foo (@PathParam("path") String path)
    {
        final Content content = loadStatic.load(path);
        return Response.ok(content.getData(), content.getType()).build();
    }
}
