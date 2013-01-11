package id.web.edom.rmi.example;

import java.io.IOException;

public interface EchoServletRemote
{
    byte[] echo (byte[] a) throws IOException;
}
