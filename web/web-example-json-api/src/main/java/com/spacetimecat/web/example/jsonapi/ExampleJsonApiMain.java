package com.spacetimecat.web.example.jsonapi;

import com.spacetimecat.web.server.DemoServer;
import com.spacetimecat.web.servlet.json.FunctionalJsonLogic;
import com.spacetimecat.web.servlet.logic.*;

import javax.json.Json;
import javax.json.JsonObject;

public final class ExampleJsonApiMain
{
    private ExampleJsonApiMain () {}

    public static void main (String[] args) throws InterruptedException
    {
        DemoServer.run(8080, new LogicServlet(logic));
    }

    private static final Logic logic =
        new Gzip(
            First.of(
                new IfMethod("POST",
                    First.of(
                        new IfPathInfo("/echo", new FunctionalJsonLogic(ExampleJsonApiMain::echo))
                        , new IfPathInfo("/add", new FunctionalJsonLogic(ExampleJsonApiMain::add))
                    )
                )
            )
        )
        ;

    private static JsonObject echo (JsonObject input)
    {
        return Json.createObjectBuilder()
            .add("message", input.getString("message"))
            .build();
    }

    private static JsonObject add (JsonObject input)
    {
        return Json.createObjectBuilder()
            .add("c", input.getInt("a") + input.getInt("b"))
            .build();
    }
}
