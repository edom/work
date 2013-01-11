package id.web.edom.rmi;

import id.web.edom.rmi.Endpoint;

public interface Java6Endpoint extends Endpoint
{
    Object wrapRemote (int object, Class<?>[] interfaces);
}
