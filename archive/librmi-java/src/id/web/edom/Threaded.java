package id.web.edom;


/**
 * <p>Indicates that the implementation owns
 * at least one {@linkplain Thread thread}.</p>
 *
 * @author erik
 */
public interface Threaded
{
    /**
     * <p>Signals all threads owned by this object to stop.
     * The threads might not stop immediately,
     * but this method should return quickly.
     * Typically this method signals the threads to stop
     * by changing a boolean flag and interrupting those threads.</p>
     *
     * <p>This method is safe to call at any time from any place,
     * any number of times, even after the threads have stopped.</p>
     *
     * @return this
     */
    Threaded stop ();

    /**
     * <p>Waits until all threads owned by this object terminates.</p>
     *
     * @return this
     *
     * @throws InterruptedException
     * if the waiting thread is {@linkplain Thread#interrupt() interrupted}
     *
     * @see Thread#join()
     */
    Threaded join () throws InterruptedException;
}
