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
        para
        tabular
        url

        local-table-of-contents
        table-of-contents
        section
        subsection
    )
    (only-in scriblib/footnote
        define-footnote
        (note       footnote)
    )
)

(provide
    ;;  --------------------    Footnotes.
    define-footnote
    fnurl
    footnote
    ;;  --------------------    Tables.
    table
)

;;  I often use this in my scrbl files.
(define (fnurl x) (footnote (url x)))

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

;;  --------------------    Sections with table of contents.

;;  Problem: How do we forward keyword arguments,
;;  including future additions, automatically,
;;  without changing the forwarding code?
(define my-section
    (make-keyword-procedure
        (lambda (kws kwargs . rest)
            (list
                ;;  2019-07-28: It surprises me that "local-table-of-contents" has to be called *before* "section"
                ;;  in order for the table of contents to appear *after* the section header.
                ;;  It surprises me even more that swapping these two statements *hides* the table of contents.
                (local-table-of-contents #:style 'immediate-only)
                (keyword-apply section kws kwargs rest)
            )
        )
    )
)
(provide (rename-out (my-section section)))

;;  Near-duplication of my-section above.
(define my-subsection
    (make-keyword-procedure
        (lambda (kws kwargs . rest)
            (list
                (local-table-of-contents #:style 'immediate-only)
                (keyword-apply subsection kws kwargs rest)
            )
        )
    )
)
(provide (rename-out (my-subsection subsection)))
