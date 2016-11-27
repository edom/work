package com.spacetimecat.web.http.negotiation;

import com.spacetimecat.web.http.MediaType;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * <p>
 *     Content negotiation functionality.
 * </p>
 */
public final class Negotiation
{
    private final List<MediaType> wanted;

    private Negotiation (List<MediaType> wanted)
    {
        Objects.requireNonNull(wanted, "wanted");
        this.wanted = wanted;
    }

    /**
     * <p>
     *     Start a negotiation.
     * </p>
     * @param acceptHeader
     * the value of the Accept header
     * @return
     * an instance
     */
    public static Negotiation withAcceptHeader (String acceptHeader)
    {
        if (acceptHeader == null)
        {
            acceptHeader = "*/*";
        }
        return new Negotiation(parse(acceptHeader));
    }

    private static List<MediaType> parse (String wanted)
    {
        String[] unparsedMediaTypes = wanted.split(",\\s*");
        return Arrays.stream(unparsedMediaTypes)
            .map(MediaType::parse)
            .filter(Objects::nonNull)
            .collect(Collectors.toList());
    }

    /**
     * <p>
     *     Find the media type that best matches the client's request.
     * </p>
     * @param supported
     * the media types supported by the server
     * @return
     * the element of the list
     * that best matches the client's request.
     * If the request doesn't have an Accept header
     * or the Accept header is malformed,
     * this returns the first element of the list.
     */
    public Result offer (List<MediaType> supported)
    {
        Objects.requireNonNull(supported, "supported");
        if (supported.isEmpty())
        {
            throw new IllegalArgumentException("supported.isEmpty()");
        }
        for (MediaType sup : supported)
        {
            for (MediaType wan : wanted)
            {
                if (sup.isSubsetOf(wan))
                {
                    return new Result(true, sup);
                }
            }
        }
        return new Result(false, supported.get(0));
    }

    public Result offer (MediaType... supported)
    {
        return offer(Arrays.asList(supported));
    }

    public Result offer (String... supported)
    {
        List<MediaType> supportedList = Arrays.stream(supported)
            .map(MediaType::parse)
            .collect(Collectors.toList());
        return offer(supportedList);
    }
}
