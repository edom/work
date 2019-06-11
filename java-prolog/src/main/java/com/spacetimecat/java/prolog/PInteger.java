package com.spacetimecat.java.prolog;

import java.math.BigInteger;
import java.util.Objects;

public final class PInteger {

    final BigInteger value;

    public PInteger (BigInteger value) {
        this.value = value;
    }

    @Override
    public int hashCode () {
        return value.hashCode();
    }

    @Override
    public boolean equals (Object that_) {
        if (!(that_ instanceof PInteger)) { return false; }
        PInteger that = (PInteger) that_;
        return Objects.equals(this.value, that.value);
    }

}
