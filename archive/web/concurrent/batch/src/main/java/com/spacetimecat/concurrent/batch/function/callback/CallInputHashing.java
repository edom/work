package com.spacetimecat.concurrent.batch.function.callback;

import com.spacetimecat.concurrent.batch.internal.call.Call;
import com.spacetimecat.concurrent.batch.internal.hash.Equivalence;
import com.spacetimecat.concurrent.batch.internal.hash.Hashing;

final class CallInputHashing<A, B> implements Hashing<Call<A, B>>
{
    private final Hashing<A> inputHashing;

    CallInputHashing (Hashing<A> inputHashing)
    {
        this.inputHashing = inputHashing;
    }

    @Override
    public int hashCodeOf (Call<A, B> call)
    {
        return inputHashing.hashCodeOf(call.get());
    }
}
