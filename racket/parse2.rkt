#lang racket

;;  -------------------- Attempt 2.

(define ((char expected) fail succ)
    (define actual (read-char))
    (if (equal? expected actual)
        (succ actual)
        (fail `("expecting" ,expected "actual" ,actual))))

(define ((string expected) fail succ)
    (define chars (string->list expected))
    (define subparsers (map char chars))
    ((apply sequence subparsers) fail succ))

(define ((seq p1 p2) fail succ)
    (p1 fail (lambda (a1)
        (p2 fail (lambda (a2)
            (succ (cons a1 a2)))))))

#;(define ((sequence . ps) fail succ)
    (match ps
        ((list)         (succ '()))
        ((cons p q)     (p fail (lambda (a)
                            ((apply sequence q) fail (lambda (b)
                                (succ (cons a b))
                            )))))))

(define (sequence . ps)
    (match ps
        ((list)         (lambda (fail succ) (succ '())))
        ((cons p q)     (seq p (apply sequence q)))))

(define ((cho a b) fail succ)
    (define port (current-input-port))
    ;;  Save port state.
    (define offset (file-position port))
    (define-values (lin col pos) (port-next-location port))
    (a  (lambda (_m)
            ;;  Backtrack (restore port state), and try next choice.
            (file-position port offset)
            (set-port-next-location! port lin col pos)
            (b fail succ))
        succ))

(define (choice . ps)
    (match ps
        ((list) (lambda (fail succ) (fail "no more choices")))
        ((cons p q) (cho p (apply choice q)))))

(define parser
    ;(sequence (char #\a) (char #\b))
    (choice
        (string "public")
        (string "static")
    )
    ;;  Problem: This only works in a lazy language?
    ;;  Problem: racket/match doesn't mix with lazy?
    ;;  What if Parser = () -> Either instead of Parser = (Succ Fail) -> Any?
    #;(choice
        (sequence (char #\a) parser)
        (char #\b)
    )
)

(with-input-from-file "Test.java" (lambda ()
    (parser (lambda (a) a) (lambda (m) m))
))
