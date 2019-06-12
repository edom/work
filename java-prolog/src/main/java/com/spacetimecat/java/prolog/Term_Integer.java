package com.spacetimecat.java.prolog;

import java.math.BigInteger;

final class Term_Integer extends Term {

    private final BigInteger value;

    private Term_Integer (BigInteger value) {
        this.value = value;
    }

    public static Term_Integer from (BigInteger value) {
        return new Term_Integer(value);
    }

    public static Term_Integer from (int value) {
        return new Term_Integer(BigInteger.valueOf(value));
    }

    public static Term_Integer from (long value) {
        return new Term_Integer(BigInteger.valueOf(value));
    }

    public int to_int () {
        return value.intValueExact();
    }

    public long to_long () {
        return value.longValueExact();
    }

    public BigInteger to_BigInteger () {
        return value;
    }

    @Override
    public int hashCode () {
        return value.hashCode();
    }

    @Override
    public boolean equals (Object that_) {
        if (!(that_ instanceof Term_Integer)) {
            return false;
        }
        if (this == that_) {
            return true;
        }
        Term_Integer that = (Term_Integer) that_;
        return this.value.equals(that.value);
    }

    @Override
    public String toString () {
        return value.toString();
    }

}
