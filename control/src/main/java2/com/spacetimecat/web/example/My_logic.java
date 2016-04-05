package com.spacetimecat.web.example;

import com.spacetimecat.control.Html;
import com.spacetimecat.web.Http_server;

/**
 * @author erik
 */
class My_logic extends Http_server.Logic
{
    interface Storage
    {
        int load_number ();
        void save_number (int x);
    }
    static class Storage_memory implements Storage
    {
        private int x;
        @Override
        public int load_number ()
        {
            return x;
        }

        @Override
        public void save_number (int x)
        {
            this.x = x;
        }
    }
    interface Strings
    {
        String good_day ();
    }
    static class English implements Strings
    {
        @Override
        public String good_day ()
        {
            return "good day";
        }
    }
    static class Indonesian implements Strings
    {
        @Override
        public String good_day ()
        {
            return "selamat siang";
        }
    }
    @Override
    public void run () throws Exception
    {
        ok();
        content_type("text/html; charset=UTF-8");
        final String name = parameter("name");
        final String lang = parameter("lang", "en");
        final Strings s;
        switch (lang)
        {
            case "en": s = new English(); break;
            case "id": s = new Indonesian(); break;
            default: s = new English(); break;
        }
        switch (target())
        {
            case "/load":
            case "/save":
            default:
        }
        final Html content = new Html();
        if (name != null)
        {
            content.text(s.good_day() + ", ").strong(name);
        }
        else
        {
            content.text("no name");
        }

        final Html html = new Html()
                .html()
                    .head()
                        .title("hello")
                        .style().attr("type", "text/css")
                            .text("body { font-family: sans-serif; font-size: 12pt; }")
                        .end()
                    .end()
                    .body()
                        .h1("Hello World: " + target())
                        .p().text(uri()).end()
                        .add(content)
                    .end()
                .end();

        write(html.render());
    }
}
