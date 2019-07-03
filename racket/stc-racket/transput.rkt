(module stc-racket-transput racket/base

    (provide

        call-with-input-file
        with-input-from-file

        current-input-port
        current-output-port
        current-error-port

        read
        read-byte
        read-char
        read-syntax

        (struct-out srcloc)

        write
        write-byte
        write-char
        display
        print

        port-count-lines!
        port-next-location

        printf
        newline
        eof-object?
    )
)
