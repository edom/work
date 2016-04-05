package com.spacetimecat.control;

/**
 * Print messages that may help you find out what is going on.
 *
 * @author erik
 */
public interface Log
{

    void info (String message);

    void error (String message);

    void error (Throwable t);

    /**
     * Log all messages to stdout or stderr, depending on severity.
     */
    class Std implements Log
    {
        @Override
        public void info (String message)
        {
            System.out.println(message);
        }

        @Override
        public void error (String message)
        {
            System.err.println(message);
        }

        @Override
        public void error (Throwable t)
        {
            t.printStackTrace(System.err);
        }
    }

    /**
     * Throw away all messages.
     */
    class None implements Log
    {
        @Override
        public void info (String message)
        {
        }

        @Override
        public void error (String message)
        {
        }

        @Override
        public void error (Throwable t)
        {
        }
    }

}
