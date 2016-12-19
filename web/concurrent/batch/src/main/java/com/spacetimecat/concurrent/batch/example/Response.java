package com.spacetimecat.concurrent.batch.example;

public final class Response
{
    public final int amount;

    public Response (int amount)
    {
        this.amount = amount;
    }

    @Override
    public String toString ()
    {
        return String.format("%s", amount);
    }
}
