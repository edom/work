package id.web.edom.rmi;

/**
 * <p>Two endpoints participate in a communication.
 * An endpoint hosts objects
 * so that their methods can be remotely invoked.
 * An endpoint is also able to ask the other endpoint
 * to execute a method in the latter's address space.</p>
 *
 * <p>All methods in this interface are thread-safe.</p>
 *
 * <p>If e1 and e2 are endpoints then e1.equals(e2) iff e1 == e2,
 * which is done by {@link Object#equals(Object)},
 * so implementors should not override that method.</p>
 *
 * @author erik
 */
public interface Endpoint
{
    class Listener
    {
        public void terminate (Endpoint e) {}
    }
    /**
     * Rejects all new remote calls.
     * The endpoint will terminate
     * as soon as all calls have returned.
     * @return this object.
     * @throws Exception
     */
    Endpoint shutdown () throws Exception;
    /**
     * Waits until this endpoint terminates.
     * @return this object.
     * @throws Exception
     */
    Endpoint join () throws Exception;
    /**
     * Performs a remote method invocation.
     * @param object remote object number
     * @param method remote method number
     * @param returnType expected type of return value
     * @param args arguments
     * @param argTypes types of arguments
     * @return the return value as given by the remote endpoint
     * @throws Throwable whatever is thrown by the remote endpoint
     */
    Object remoteInvoke (int object, int method, final Class returnType, Object[] args, Class[] argTypes) throws Exception;
}
