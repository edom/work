package com.spacetimecat.java.prolog2.parser;

import java.util.function.Function;
import java.util.function.Supplier;

public abstract class Either<A, B> {

    public static <A, B> Either<A, B> from_nullable (Supplier<A> if_null, B value) {
        return (value == null) ? left(if_null.get()) : right(value);
    }

    public static <A, B> Either<A, B> left (A value) {
        return new Left<>(value);
    }

    public static <A, B> Either<A, B> right (B value) {
        return new Right<>(value);
    }

    public abstract <C> C fold (Function<A, C> f, Function<B, C> g);

    public final <C> Either<A, C> map_right (Function<B, C> f) {
        return fold(a -> left(a), b -> right(f.apply(b)));
    }

    public static <A, B> Either<A, B> join_right (Either<A, Either<A, B>> mm) {
        return mm.fold(a -> left(a), e -> e);
    }

    public final <C> Either<A, C> then_right (Function<B, Either<A, C>> that) {
        return fold(a -> left(a), b -> that.apply(b));
    }

    private Either () {
    }

    private static final class Left<A, B> extends Either<A, B> {

        final A value;

        private Left (A value) {
            this.value = value;
        }

        @Override
        public <C> C fold (Function<A, C> f, Function<B, C> g) {
            return f.apply(value);
        }

    }

    private static final class Right<A, B> extends Either<A, B> {

        final B value;

        private Right (B value) {
            this.value = value;
        }

        @Override
        public <C> C fold (Function<A, C> f, Function<B, C> g) {
            return g.apply(value);
        }

    }

}
