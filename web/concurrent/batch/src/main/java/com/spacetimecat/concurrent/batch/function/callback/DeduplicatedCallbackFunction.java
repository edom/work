package com.spacetimecat.concurrent.batch.function.callback;

import com.spacetimecat.concurrent.batch.internal.call.Call;
import com.spacetimecat.concurrent.batch.internal.call.Calls;
import com.spacetimecat.concurrent.batch.internal.hash.CustomHashed;
import com.spacetimecat.concurrent.batch.internal.hash.Equivalence;
import com.spacetimecat.concurrent.batch.internal.hash.Hashing;
import com.spacetimecat.java.lang.callback.CallbackFunction;
import com.spacetimecat.java.lang.callback.ListCallbackFunction;
import com.spacetimecat.java.lang.consumer.GatheringConsumer;
import com.spacetimecat.java.lang.group.ByHash;

import java.util.Arrays;
import java.util.List;
import java.util.function.Consumer;
import java.util.stream.Collectors;

/**
 * <p>
 *     Groups calls with equivalent inputs into one call.
 *     Avoids repeating calls with equivalent inputs.
 * </p>
 *
 * @param <A>
 * input list element type
 *
 * @param <B>
 * output list element type
 */
public final class DeduplicatedCallbackFunction<A, B> implements ListCallbackFunction<A, B>
{
    private final Equivalence<A> inputEquivalence;
    private final Hashing<A> inputHashing;
    private final CallbackFunction<List<A>, List<B>> underlying;

    /**
     * <p>
     *     Groups calls with equivalent inputs into one call.
     *     Avoids repeating calls with equivalent inputs.
     * </p>
     *
     * @param inputEquivalence
     * determines whether two inputs are considered equivalent
     *
     * @param inputHashing
     * computes the hash of an input
     *
     * @param underlying
     * does the real work;
     * must satisfy the contract of {@link ListCallbackFunction}
     */
    public DeduplicatedCallbackFunction
    (
        Equivalence<A> inputEquivalence
        , Hashing<A> inputHashing
        , CallbackFunction<List<A>, List<B>> underlying
    )
    {
        this.inputEquivalence = inputEquivalence;
        this.inputHashing = inputHashing;
        this.underlying = underlying;
    }

    @SuppressWarnings("unchecked")
    @Override
    public void apply (List<A> inputs, Consumer<List<B>> outputConsumer)
    {
        if (inputs.isEmpty()) { return; }

        final int size = inputs.size();

        final Call<A, B>[] calls = new Call[size];
        final GatheringConsumer<B> gatherer =
            new GatheringConsumer<>(size, outputConsumer);

        for (int i = 0; i < size; ++i)
        {
            calls[i] = new Call<>(inputs.get(i), gatherer.get(i));
        }

        final Equivalence<Call<A, B>> callEquivalence = new CallInputEquivalence<>(inputEquivalence);
        final Hashing<Call<A, B>> callHashing = new CallInputHashing<>(inputHashing);

        final List<CustomHashed<Call<A, B>>> customCalls =
            Arrays.stream(calls)
                .map(call -> new CustomHashed<>(callEquivalence, callHashing, call))
                .collect(Collectors.toList());

        final List<List<Call<A, B>>> equivalenceClasses =
            new ByHash<CustomHashed<Call<A, B>>>().group(customCalls).stream()
                .map(CustomHashed::unwrapList)
                .collect(Collectors.toList());

        final List<Call<A, B>> groupedCalls =
            equivalenceClasses.stream()
                .map(Call::fromEquivalenceClass)
                .collect(Collectors.toList());

        new Calls<>(groupedCalls).runOn(underlying);
    }
}
