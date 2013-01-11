package id.web.edom.rmi;

import id.web.edom.Threaded;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;

import org.slf4j.Logger;

/**
 * <p>Dispatches a call using reflection to a table of tables of methods,
 * possibly to a different thread.</p>
 *
 * @author erik
 */
class DefaultDispatch implements Dispatch
{
    private static final Logger ld = Log.getDispatch();
    private final ExecutorService executor;
    private final Object[] objects;
    private final Method[][] table;

    /**
     * @param objects the exported objects
     * @param executor will execute calls
     */
    DefaultDispatch (Object[] objects, ExecutorService executor)
    {
        if (ld.isDebugEnabled())
        {
            ld.debug("Starting {} objects:{} executor:{}.", this, Arrays.toString(objects), executor);
        }
        final int n = objects.length;
        this.executor = executor;
        this.objects = objects;
        this.table = new Method[n][];
        for (int i = 0; i < n; ++i)
        {
            table[i] = objects[i].getClass().getDeclaredMethods();
        }
    }

    private void checkObject (int object) throws Exception
    {
        if ((object < 0) || (object >= objects.length)) throw new InvalidObjectNumberException(object);
    }

    private void checkMethod (int object, int method) throws Exception
    {
        if ((method < 0) || (method >= table[object].length)) throw new InvalidMethodNumberException(method);
    }

    private Object getObject (int object) throws Exception
    {
        return objects[object];
    }

    private Method getMethod (int object, int method) throws Exception
    {
        return table[object][method];
    }

    @SuppressWarnings("rawtypes")
    public Class[] getParameterTypes (int object, int method) throws Exception
    {
        checkObject(object);
        checkMethod(object, method);
        return getMethod(object, method).getParameterTypes();
    }

    @Override
    public void dispatch (final int object, final int method, final Object[] args, final Dispatch.Listener re) throws Exception
    {
        if (ld.isTraceEnabled())
        {
            ld.trace("Call object:{} method:{} args:{}.", object, method, args);
        }
        checkObject(object);
        if (method == SpecialMethods.LOOKUP)
        {
            final String name = (String) args[0];
            final int n = lookupMethod(object, name);
            re.ok(n, int.class);
            return;
        }
        checkMethod(object, method);
        final Object o = getObject(object);
        final Method m = getMethod(object, method);
        final Runnable runnable = new Runnable()
        {
            @Override
            public void run ()
            {
                try
                {
                    final Object r = m.invoke(o, args);
                    if (ld.isTraceEnabled())
                    {
                        ld.trace("Return object:{} method:{} value:{}.", object, method, r);
                    }
                    re.ok(r, m.getReturnType());
                }
                catch (InvocationTargetException e)
                {
                    final Throwable f = e.getCause();
                    if (ld.isDebugEnabled())
                    {
                        ld.debug("Called method threw a throwable.", f);
                    }
                    re.error(e.getCause());
                }
                catch (Exception e)
                {
                    ld.error("Invocation failed.", e);
                    re.error(e);
                }
            }
        };
        executor.submit(runnable);
    }

    public int lookupMethod (int object, String name) throws NoSuchMethodException
    {
        if (ld.isDebugEnabled())
        {
            ld.debug("Looking up method \"{}\" in object {}.", name, object);
        }
        final int n = table[object].length;
        for (int i = 0; i < n; ++i)
        {
            final Method m = table[object][i];
            final String mName = m.getName();
            if (mName.equals(name)) { return i; }
        }
        throw new NoSuchMethodException(name);
    }

    public Threaded stop() { executor.shutdown(); return this; }

    public Threaded join () throws InterruptedException
    {
        while (!executor.awaitTermination(1, TimeUnit.DAYS)) {}
        return this;
    }
}
