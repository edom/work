#lang s-exp "lang.rkt"

(define-syntax-class number%
    #:attributes (n)
    (pattern n:number)
)

(define-syntax-class pair%
    #:attributes (a b)
    ;;  #:attributes (a a.n b b.n)
    (pattern [a:number% b:number%])
)

;;  2019-10-07:
;;  Surprise: "syntax-class" attribute nesting is not recursive,
;;  perhaps because the implementation would be too hard.

(syntax-parse #'(_ (1 2))
    [   (_ pair:pair%)
        ;;  Surprise: This raises an error.
        #'(list pair.a.n)
    ]
)
