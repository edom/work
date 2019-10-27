#lang s-exp "lang.rkt"

(require
    racket/pretty
    rackunit
    rackunit/text-ui
    "../racket/path.rkt"
    "all.rkt"
)

(provide main)

;;  XXX: ".." is an abuse.
(define test-data-path (build-path/simplify $__FILE__/path ".." "test-data.rkt"))

(define-test-suite quick-tests
    (test-case "Detect obvious security problems"
        ;;  See also the notes in this file.
        (define path "com-spacetimecat/analyze/test-restrict.rkt")
        (test-exn "The BOOM file should not get created"
            exn:fail:unsupported?
            (λ -> (make-namespace-from-file path)))
        (define remedy "
Please delete the BOOM file and rerun the test.
If the file gets re-created or this message persists,
then there may be a security problem or a programming error.
")
        (with-check-info (['remedy (string-info remedy)])
            (check-false (file-exists? "BOOM")))
    )
    (test-case "Outline"
        (define outline (compute-file-outline-1 test-data-path))
        (pretty-print (syntax->datum (datum->syntax #f outline)))
        ;(pretty-print outline)
    )
    (test-case "Namespace"
        (define namespace (make-namespace-from-file test-data-path))
        (check-not-false (member 'f (namespace-mapped-symbols namespace)))
    )
)

(define (main) (run-tests quick-tests))
