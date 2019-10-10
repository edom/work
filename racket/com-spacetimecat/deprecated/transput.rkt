(module stc-racket-transput racket/base

    ;;  TODO: Turn this into a guide/tutorial.

    (require
        racket/path
    )

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

        ;;  see also syntax.rkt

        read-accept-reader

        ;;  file-system paths

        path?

        string->path
        path->string

        path->complete-path
        simple-form-path
        explode-path

        current-directory
    )
)
