#lang racket

(require "racket-extra.rkt")

(define-syntax-rule (example Description Expression ...)
    (begin
        (printf "~n--------------------    ~a~n" Description)
        (begin
            (newline)
            (pretty-print 'Expression)
            (display "-->\n")
            (pretty-print Expression)
            (newline)
        )
        ...
    ))

(example "Lambda expression with fewer parentheses"
    (λ a b c -> (+ a b c))
    [(λ a b c -> (+ a b c)) 1 2 3]
)
(example "Lambda expression with implicitly-parenthesized body"
    (λ a b c => + a b c)
    [(λ a b c => + a b c) 1 2 3]
)
(example "LET-like forms with fewer parentheses"
    (
        LET*	a   1
                b   2
                c   3
                ab  (+ a b)
                abc (+ ab c)
        IN  (+ abc 4)
    )
)

(define messages [list
	(hash 'id 0 'sender 'joe)
	(hash 'id 1 'sender 'bob)
	(hash 'id 2 'sender 'joe)
	(hash 'id 3 'sender 'joe)
	(hash 'id 4 'sender 'bob)
	(hash 'id 5 'sender 'alice)
])

(example "The 'messages' variable for GROUP BY examples below"
    messages
)
(example "GROUP BY FUNCTION"
    (GROUP LIST messages BY FUNCTION (λ e => hash-ref e 'sender))
)
(example "GROUP BY HASH KEY (equivalent to the previous example)"
    (GROUP LIST messages BY HASH KEY sender)
)
(example "GROUP BY DICT KEY (somewhat more general than the previous example)"
    (GROUP LIST messages BY DICT KEY sender)
)
(example "GROUP VECTOR BY DICT KEY"
    (GROUP VECTOR (list->vector messages) BY DICT KEY sender)
)
(example "Count-aggregation"
    (hash-map
        (GROUP LIST messages BY DICT KEY sender)
        [λ k v => cons k (length v)]
    )
)
