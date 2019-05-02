package com.spacetimecat.web.http.negotiation;

import com.spacetimecat.web.http.MediaType;
import com.spacetimecat.web.http.NotAcceptableException;

import java.util.Objects;

/**
 * <p>
 *     Content negotiation result.
 * </p>
 */
public final class Result
{
    private final boolean accepted;
    private final MediaType mediaType;

    Result (boolean accepted, MediaType mediaType)
    {
        Objects.requireNonNull(mediaType, "mediaType");
        this.accepted = accepted;
        this.mediaType = mediaType;
    }

    public boolean isAccepted ()
    {
        return accepted;
    }

    /**
     * <p>
     *     Return the media type that best satisfies the client's constraints,
     *     even if the server can't satisfy those.
     * </p>
     * @return see description
     */
    public MediaType lenient ()
    {
        return mediaType;
    }

    /**
     * <p>
     *     Return the media type that best satisfies the client's constraints,
     *     or throw a {@link NotAcceptableException}.
     * </p>
     * @return see description
     * @throws NotAcceptableException
     * if the server can't satisfy the client's constraints
     */
    public MediaType strict ()
    {
        if (!accepted)
        {
            throw new NotAcceptableException();
        }
        return mediaType;
    }
}
