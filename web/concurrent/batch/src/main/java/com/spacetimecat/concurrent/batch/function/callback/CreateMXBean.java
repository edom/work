package com.spacetimecat.concurrent.batch.function.callback;

import com.spacetimecat.concurrent.batch.internal.BatcherMXBean;
import com.spacetimecat.concurrent.batch.internal.BatcherMXBeanImp;

/**
 * <p>
 *     Experimental; for use with Java Management Extensions (JMX).
 * </p>
 */
public final class CreateMXBean
{
    private CreateMXBean () {}

    public static BatcherMXBean forInstance (TimeBatchedCallbackFunction function)
    {
        return new BatcherMXBeanImp(function.batchByTime);
    }
}
