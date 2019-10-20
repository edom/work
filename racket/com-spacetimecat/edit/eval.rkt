#lang s-exp "lang.rkt"

(require
    racket/port
)

(provide
    eval/string
)

;;  This is like read-eval-print-loop, but this reads from the given string
;;  and writes output and error to the returned string.
(define (eval/string input-str [namespace (current-namespace)])
    (call-with-input-string input-str (λ input ->
        (port-count-lines! input)
        (call-with-output-string (λ output ->
            (parameterize ( [current-namespace namespace]
                            [current-input-port input]
                            [current-output-port output]
                            [current-error-port output] )
                (my-repl)))))))

(define (print-values . values)
    (list-for-each (current-print) values))

(define (show-exn-ish e)
    (if (exn? e)
        ((error-display-handler) (exn-message e) e)
        (printf "unhandled exception: ~a~n~n(Stack traces are only available when throwing instances of exn.)~n" e)))

;;  Curious Question: Do tags get garbage-collected?
(define tag-abort (make-continuation-prompt-tag))

;;  Horrible Confusing Problem with continuations: 2019-10-18:
;;  I don't use read-eval-print-loop because I can't get it to print exceptions to my string.
;;  Used inside a GUI event handler, eval-syntax throws an exception for undefined symbols.
;;  Used outside, it doesn't.
;;  Probably something crashes into some continuation barrier somewhere.
;;  https://lists.racket-lang.org/users/archive/2010-July/040393.html
(define (my-repl)
    (call-with-continuation-prompt
        (λ -> (call-with-exception-handler my-handler loop))
        tag-abort
        void))

(define (my-handler e)
    (show-exn-ish e)
    (abort-current-continuation tag-abort))

(define (loop)
    (define stx-0 (read-syntax))
    (unless (eof-object? stx-0)
        (define dat (cons '#%top-interaction stx-0))
        (define stx-1 (datum->syntax #f dat stx-0))
        (define stx-2 (namespace-syntax-introduce stx-1))
        (call-with-continuation-prompt
            (λ -> (eval+print stx-2))
            (default-continuation-prompt-tag)
            print-values)
        (loop)))

(define (eval+print stx)
    (call-with-values
        (λ -> (eval-syntax stx))
        print-values))
