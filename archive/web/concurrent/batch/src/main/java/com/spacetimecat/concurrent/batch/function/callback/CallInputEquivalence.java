package com.spacetimecat.concurrent.batch.function.callback;

import com.spacetimecat.concurrent.batch.internal.call.Call;
import com.spacetimecat.concurrent.batch.internal.hash.Equivalence;

final class CallInputEquivalence<A, B> implements Equivalence<Call<A, B>>
{
    private final Equivalence<A> inputEquivalence;

    CallInputEquivalence (Equivalence<A> inputEquivalence)
    {
        this.inputEquivalence = inputEquivalence;
    }

    @Override
    public boolean areEquivalent (Call<A, B> a, Call<A, B> b)
    {
        return inputEquivalence.areEquivalent(a.get(), b.get());
    }
}
