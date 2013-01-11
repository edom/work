package id.web.edom.rmi;

import id.web.edom.rmi.Endpoint;
import id.web.edom.rmi.SpecialMethods;

import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.util.Map;
import java.util.TreeMap;

/**
 * <p>Allows remote objects to be used as if it were local object.</p>
 *
 * @author erik
 */
public class ProxyInvocationHandler implements InvocationHandler
{
    private final Map<String, Integer> map = new TreeMap<String, Integer>();
    private final Endpoint endpoint;
    private final int object;

    public ProxyInvocationHandler (int object, Endpoint endpoint)
    {
        this.object = object;
        this.endpoint = endpoint;
    }

    @Override
    public Object invoke (Object proxy, Method method, Object[] args) throws Throwable
    {
        final String name = method.getName();
        Integer number;
        synchronized (map) { number = map.get(method.getName()); }
        if (number == null)
        {
            number = (Integer) endpoint.remoteInvoke(object, SpecialMethods.LOOKUP, int.class, new Object[]{name}, new Class<?>[]{String.class});
            synchronized (map) { map.put(name, number); }
        }
        return endpoint.remoteInvoke(object, number, method.getReturnType(), args, method.getParameterTypes());
    }

}
