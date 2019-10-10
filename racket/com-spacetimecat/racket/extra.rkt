#lang s-exp "lang.rkt"

(require
    "require-provide.rkt"
)

(require+provide
    racket/dict
    racket/list
    racket/pretty
    "date.rkt"
    "fewer-parens.rkt"
    "string.rkt"
    "system.rkt"
)

(provide

    (all-from-out "require-provide.rkt")

    ;;  Destructuring bind.

    DECONSTRUCT
    alist-ref

    ;;  Grouping/Partitioning.

    GROUP

)

;;  --------------------    Destructuring bind.

;;  Create local variables that refer to the object properties with the same name.
;;  "Getter" is usually hash-ref or alist-ref.
;;  See also Racket "match".
;;  See also Common Lisp "destructuring-bind".
;;  See also TypeScript destructuring assignment.
;;  The advantage of "DECONSTRUCT" over "match"
;;  is that "DECONSTRUCT" requires typing the key once
;;  whereas "match" requires typing the key twice.
;;  Compare:
;;  (DECONSTRUCT h WITH hash-ref TO (k1 k2 k3))
;;  with:
;;  (match h (hash-table ('k1 k1) ('k2 k2) ('k3 k3)))

(define-syntax-parser DECONSTRUCT
    #:datum-literals (WITH FROM TO)
    [(_ Table WITH Getter TO (Var ...))
        #'(define-values (Var ...) (values (Getter Table 'Var) ...))]
    [(_ Table WITH Getter FROM (Index ...) TO (Var ...))
        #'(define-values (Var ...) (values (Getter Table Index) ...))])

(define (alist-ref alist key)
    (define pair (assoc key alist))
    (if pair
        (cdr pair)
        (error 'alist-ref "undefined key: ~a" key)))

;;  --------------------    Grouping/Partitioning.

(begin-for-syntax
    (define-splicing-syntax-class Group_Source
        #:datum-literals (LIST VECTOR)
        [pattern (~seq LIST Source)]
        [pattern (~seq VECTOR Source)]
    )
    (define-splicing-syntax-class Group_By
        #:datum-literals (ASSOC BY DICT FUNCTION HASH KEY LIST)
        [pattern (~seq BY FUNCTION Key_Func)]
        [pattern (~seq BY DICT KEY Prop_Name) #:with Key_Func #'(λ x => dict-ref x `Prop_Name)]
        [pattern (~seq BY HASH KEY Prop_Name) #:with Key_Func #'(λ x => hash-ref x `Prop_Name)]
        [pattern (~seq BY ASSOC KEY Prop_Name) #:with Key_Func #'(λ x => cdr [assoc `Prop_Name x])]
    )
)

(define-syntax-parser GROUP
    [(_ Source:Group_Source By:Group_By)
        #'(_group By.Key_Func Source.Source)]
)

(define (_group Key_Func Iterable)
    (define Groups (make-hash))
    ;;  Do we need in-list or in-vector?
    ;;  Do they improve performance?
    (for ([Elem Iterable])
        (define key (Key_Func Elem))
        (hash-update! Groups key (λ Old => cons Elem Old) '()))
    Groups
)
