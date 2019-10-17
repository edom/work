#lang s-exp "lang.rkt"

(require
    racket/pretty
    rackunit
    rackunit/text-ui
    "all.rkt"
)

(provide main)

(define-test-suite quick-tests
    (test-case "Detect obvious security problems"
        ;;  See also the notes in this file.
        (define path "com-spacetimecat/analyze/test-restrict.rkt")
        (test-exn "The BOOM file should not get created"
            exn:fail:unsupported?
            (λ => make-namespace-from-file path))
        (define remedy "
Please delete the BOOM file and rerun the test.
If the file gets re-created or this message persists,
then there may be a security problem or a programming error.
")
        (with-check-info (['remedy (string-info remedy)])
            (check-false (file-exists? "BOOM")))
    )
    (test-case "Outline"
        ;;  XXX: ".." is an abuse.
        (define path (simplify-path (build-path $__FILE__/path ".." "test-data.rkt") #f))
        (define outline (compute-file-outline-1 path))
        (pretty-print (syntax->datum (datum->syntax #f outline)))
        ;(pretty-print outline)
    )
)

(define (main) (run-tests quick-tests))
