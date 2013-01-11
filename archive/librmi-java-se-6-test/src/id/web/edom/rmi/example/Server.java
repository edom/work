package id.web.edom.rmi.example;

import id.web.edom.rmi.EndpointBuilder;
import id.web.edom.rmi.EndpointBuilderFactory;
import id.web.edom.rmi.server.RmiServer;

import java.net.InetSocketAddress;
import java.net.SocketAddress;

public class Server
{
    public static int PORT = 1024;
    public static void main (String[] args) throws Exception
    {
        final int maxEndpoints = 1024;
        final SocketAddress address = new InetSocketAddress(PORT);
        final EndpointBuilderFactory factory = new EndpointBuilderFactory()
        {
            @Override
            public EndpointBuilder createEndpointBuilder ()
            {
                return new EndpointBuilder().export(new EchoServlet());
            }
        };
        RmiServer.create("ExampleRmiServer", factory, maxEndpoints, address).join();
    }
}
