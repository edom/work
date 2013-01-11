package id.web.edom.rmi.example;

public class EchoServlet implements EchoServletRemote
{
    @Override
    public byte[] echo (byte[] a)
    {
        return a;
    }
}
