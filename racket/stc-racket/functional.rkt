;;  functional programming

;;  Scheme is already mostly functional.

(module stc-racket-functional racket/base

    (require
        racket/match
        racket/stream
    )

    (provide

        ;;  pattern-matching that can emulate Haskell algebraic data types with Scheme structs

        define/match
        match
        match-define
        match-let

        ;;  streams, expected to be lazy
        ;;
        ;;  racket/stream conflicts with SRFI-41.
        ;;  https://stackoverflow.com/questions/14979972/are-srfi-41-and-racket-stream-different
        ;;
        ;;  Should we use Lazy Racket lists instead of Racket streams?
        ;;  https://groups.google.com/forum/#!topic/racket-users/iFiBiU-YNhI

        stream?
        empty-stream
        stream-cons
        stream-map
        stream-first
        stream-rest
        stream->list
    )
)
