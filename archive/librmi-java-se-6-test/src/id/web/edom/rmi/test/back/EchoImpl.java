package id.web.edom.rmi.test.back;

public class EchoImpl implements Echo
{
    @Override
    public byte[] echo (byte[] b)
    {
        return b;
    }
}
