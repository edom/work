#lang s-exp "lang.rkt"

(provide
    call-with-restrictions
    begin-with-restrictions
)

;;  See also the notes in test-restrict.rkt.

;;  The caller must not depend on the type of the thrown exception.
;;
;;  2019-10-17:
;;      -   Should we use sandboxes?
;;      -   Should we make a new exception type?

(define (call-with-restrictions potentially-harmful-thunk)
    (parameterize ([current-security-guard (make-my-guard)])
        (potentially-harmful-thunk)))

(define-syntax-rule (begin-with-restrictions body ...)
    (call-with-restrictions (lambda () body ...)))

;;  2019-10-17: Should we allow exists+read for all paths?

(define (make-my-guard)
    (define allow void)
    (define (deny proc fmt . args)
        (define msg (format "~a: ~a" proc (apply format fmt args)))
        (define exc (make-exn:fail:unsupported msg (current-continuation-marks)))
        (raise exc))
    (define (file-guard proc path accesses)
        (if (or (equal? accesses '(exists))
                (equal? accesses '(read)))
            (allow)
            (deny proc "file-guard: ~s" (list 'path path 'accesses accesses))))
    (define (network-guard proc host port role)
        (deny proc "network-guard: ~s" (list 'host host 'port port 'role role)))
    (make-security-guard (current-security-guard) file-guard network-guard))
