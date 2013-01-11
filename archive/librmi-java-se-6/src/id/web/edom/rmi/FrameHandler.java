package id.web.edom.rmi;

import id.web.edom.rmi.Frame;

/**
 * Decides what to do with a {@link Frame}.
 * @author erik
 */
public interface FrameHandler
{
    void handle (Frame f) throws Exception;
}
