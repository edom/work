package id.web.edom.rmi.server;

import java.io.IOException;

import javax.management.MXBean;

/**
 * <p>Instrumentation for {@link RmiServer}.</p>
 *
 * @author erik
 */
@MXBean
public interface RmiServerMXBean
{
    String getAddress () throws IOException;
    double getAcceptanceRatio () throws IOException;
    long getAcceptedConnections () throws IOException;
    long getServedConnections () throws IOException;
    int getOpenConnections () throws IOException;
    long getBytesRead () throws IOException;
    long getBytesWritten () throws IOException;
    int getMaxConnections () throws IOException;
    int getMaxFrameSize () throws IOException;
    void setMaxFrameSize (int n) throws IOException;
    void setMaxConnections (int n) throws IOException;
    void stop () throws IOException;
    String getStartTime () throws IOException;
    String getUptime () throws IOException;
}
