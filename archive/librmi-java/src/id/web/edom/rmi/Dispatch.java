package id.web.edom.rmi;

import id.web.edom.Threaded;

import java.util.concurrent.RejectedExecutionException;

/**
 * <p>A collection of exported objects.
 * Schedules calls or performs calls.
 * Internal protocol handler.</p>
 *
 * @author erik
 */
public interface Dispatch extends Threaded
{
    /**
     * <p>Receives the return value of a possibly asynchronous dispatch.</p>
     *
     * @author erik
     */
    interface Listener
    {
        /**
         * <p>Normal return.</p>
         *
         * @param value the return value
         * @param type the type of the return value
         */
        void ok (Object value, Class type);
        /**
         * <p>Abnormal return.</p>
         *
         * @param t cause of the abnormal return
         */
        void error (Throwable t);
    }

    /**
     * <p>Gets the parameter types of the method.</p>
     *
     * @param object the object number
     * @param method the method number
     * @return the parameter types
     * @throws Exception
     */
    Class[] getParameterTypes (int object, int method) throws Exception;

    /**
     * <p>Possibly asynchronous dispatch.</p>
     *
     * @param object the object number
     * @param method the method number
     * @param args the arguments
     * @param re will be called when the called method returns
     *
     * @throws RejectedExecutionException if queue is full, server is busy, or server is stopping
     * @throws Exception
     */
    void dispatch (int object, int method, Object[] args, Listener re) throws Exception;
}
