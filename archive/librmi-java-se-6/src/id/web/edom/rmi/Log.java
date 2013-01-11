package id.web.edom.rmi;

import java.nio.channels.SelectableChannel;
import java.nio.channels.SocketChannel;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>Loggers.</p>
 *
 * @author erik
 */
public final class Log
{
    private static final String ROOT = "id.web.edom";
    private static final String RMI = ROOT + ".rmi";
    private static final String SERVER = RMI + ".server";
    private static final String CONNECTION = RMI + ".connection";
    private static final String DISPATCH = RMI + ".dispatch";
    private static final String TRANSPUT = RMI + ".transput";
    private static final String QUEUE = RMI + ".queue";
    private static final String SELECTOR = RMI + ".selector";
    private static final Logger lc = LoggerFactory.getLogger(CONNECTION);
    public static Logger getServer () { return LoggerFactory.getLogger(SERVER); }
    public static Logger getDispatch () { return LoggerFactory.getLogger(DISPATCH); }
    public static Logger getTransput () { return LoggerFactory.getLogger(TRANSPUT); }
    public static Logger getQueue () { return LoggerFactory.getLogger(QUEUE); }
    public static Logger getSelector () { return LoggerFactory.getLogger(SELECTOR); }

    public static void accept (SocketChannel c)
    {
        if (lc.isInfoEnabled())
        {
            lc.info("accept {}", c);
        }
    }

    public static void timeout (SelectableChannel c)
    {
        if (lc.isInfoEnabled())
        {
            lc.info("Killing idle connection {}.", c);
        }
    }

    public static void close (SelectableChannel c)
    {
        if (lc.isInfoEnabled())
        {
            lc.info("close {}", c);
        }
    }

    private Log () {}
}
