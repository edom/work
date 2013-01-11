package id.web.edom.rmi.server;

import java.io.IOException;

public interface EndpointMXBean
{
    void close () throws IOException;
    String getRemoteAddress () throws IOException;
    String getSince () throws IOException;
    String getUptime () throws IOException;
    long getBytesSent () throws IOException;
    long getBytesReceived () throws IOException;
}
