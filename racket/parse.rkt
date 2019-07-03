#lang s-exp "stc-racket.rkt"

;;  We want both parser and unparser, like in Rendel & Ostermann 2010
;;  "Invertible Syntax Descriptions: Unifying Parsing and Pretty Printing":
;;  http://www.informatik.uni-marburg.de/~rendel/unparse/rendel10invertible.pdf

;;  Lazy-reading simplifies backtracking.

(define-struct Event (content location) #:transparent)

(define (port->stream path port)
    (port-count-lines! port)
    (define (next)
        (define-values (line column byte) (port-next-location port))
        (define char (read-char port))
        (define source path)
        (define span #f)
        (define location (make-srcloc source line column byte span))
        (if (eof-object? char)
            empty-stream
            (stream-cons (Event char location) (next))
        )
    )
    (next)
)

;;  Parser = Stream Succ Fail -> Any
;;  Succ = Event Stream -> Any
;;  Fail = Message -> Any

;;  char : Char -> Parser
(define ((char expected) stream succ fail)
    (define event (stream-first stream))
    (define actual (Event-content event))
    (if (equal? expected actual)
        (succ event (stream-rest stream))
        (fail `("expecting" ,expected "actual" ,actual))
    )
)

;;  test

(let ((path "Test.java"))
    (define parser (char #\p))
    (call-with-input-file "Test.java" (lambda (port)
        (define stream (port->stream "Test.java" port))
        ;; test dump
        (for ((x (in-stream stream)))
            (printf "~v~n" x)
        )
    ))
)
