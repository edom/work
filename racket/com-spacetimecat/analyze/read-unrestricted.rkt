#lang s-exp "lang.rkt"

;;  If you can, use read.rkt, and avoid these unrestricted procedures.

(provide
    read-module-from-file
    make-namespace-from-file
)

;;  Security Note: #lang and #reader can execute arbitrary code.

(define (read-module-from-file path)
    (parameterize [ (read-accept-lang #t)
                    (read-accept-reader #t) ]
        (read-syntax-from-file path)))

(define (make-namespace-from-file path)
    (parameterize ([current-namespace (make-base-namespace)])
        (dynamic-require path #f)
        (current-namespace)))
