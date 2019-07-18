#lang racket/base

(require
    (only-in racket/list
        empty
        empty?
    )
    (only-in scribble/base
        bold
        centered
        hspace
        nested
        tabular
    )
    (only-in scriblib/footnote
        define-footnote
        (note       footnote)
    )
)

(provide
    define-footnote
    footnote
    table
)

;;  --------------------    Tables in row-major format.
;;
;;  Because Scribble's "tabular" function is unsatisfactory.
;;
;;  But this is not completely satisfactory either:
;;  This does not handle complex table layouts.
;;
;;  Problem:
;;  "(centered (tabular ...))" does not work.
;;  The cause may be related to Scribble's emitting HTML "blockquote" tags.

(define
    (table
        #:header (header empty)     ;;  Listof pre-content?
        #:caption (caption #f)      ;;  U String False
        #:rows rows                 ;;  Listof (Listof pre-content?)
    )
    (nested
        (tabular
            #:style 'centered
            #:row-properties '((bottom-border) ())
            #:sep (hspace 1)
            (append
                (if (empty? header)
                    empty
                    (list (map bold header))
                )
                rows
            )
        )
        (if caption
            (string-append "Table: " caption)
            ""
        )
    )
)
