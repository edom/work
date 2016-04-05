package com.spacetimecat.web.example;

import com.spacetimecat.control.Log;
import com.spacetimecat.web.Http_server;

/**
 * @author erik
 */
public class Http_example
{

    public static void main (String[] args) throws Exception
    {
        final Log log = new Log.Std();
        final Http_server s = new Http_server(log, 8080);
        s.logic(new My_logic());
        s.static_("/home/erik/hak2/sites/spacetimecat.com/_site");
        s.run();
        // jetty 9.2 requires java 7.
        // jetty 9.3 requires java 8.
    }
}
