#lang racket/base

;;  --------------------    Some enhancements.

(require
    (for-syntax
        racket/base
        syntax/parse
    )
    syntax/parse/define
    "require-provide.rkt"
)

(require+provide
    racket/date
    racket/format
    racket/list
    racket/port
    racket/system
)

(provide

    (all-from-out "require-provide.rkt")

    ;;  Forms with fewer parentheses.

    LET LET* LETREC LETREC*
    λ   ;;  This conflicts with Racket's λ.
    !=
    not-equal?

    ;;  Destructuring bind.

    DECONSTRUCT
    alist-ref

    ;;  Date.

    current-date
    format-date/iso

    ;;  Everything-to-string.

    ->string

    ;;  Grouping/Partitioning.

    GROUP

    ;;  Operating system.

    system**/exit-code
    with-environment

)

;;  --------------------    LET-like forms with fewer parentheses.

(define-syntax-parser _GLET
    #:datum-literals (IN)
    [(_ Let (~seq Name Expr) ... IN Body ...)
        #'(Let ((Name Expr) ...) Body ...)])

(define-syntax-rule (LET Arg ...) (_GLET let Arg ...))
(define-syntax-rule (LET* Arg ...) (_GLET let* Arg ...))
(define-syntax-rule (LETREC Arg ...) (_GLET letrec Arg ...))
(define-syntax-rule (LETREC* Arg ...) (_GLET letrec* Arg ...))

(define (!= a b)                (not (= a b)))
(define (not-equal? a b)        (not (equal? a b)))

;;  --------------------    Operating system.

;;  This is like system*/exit-code but this automatically converts all strings to bytes with UTF-8 encoding.

(define (system**/exit-code path . args)
    (define (->bytes x) (string->bytes/utf-8 (->string x)))
    (apply system*/exit-code path (map ->bytes args)))

;;  This provides let-like syntax for parameterize-ing current-environment-variables.
;;  Keys are automatically quoted.
;;  Everything is converted to string.
;;  Example:
;;      (with-environment ([foo 'bar] [baz 1]) (displayln 'hello))

(define-syntax-rule (with-environment ((Key Value) ...) Thunk ...)
    (let ((env (make-environment-variables)))
        (environment-variables-set! env
            (string->bytes/utf-8 (->string 'Key))
            (string->bytes/utf-8 (->string Value))
        ) ...
        (parameterize ((current-environment-variables env)) Thunk ...)))

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

;;  --------------------    Conversion to string.

;;  Convert any Racket value to string.
;;  Unknown values are displayed to a string.

(define (->string x)
    (cond   [(string? x) x]
            [(number? x) (number->string x)]
            [(symbol? x) (symbol->string x)]
            [else (call-with-output-string
                (lambda (port) (display x port)))]))

;;  --------------------	Lambda expressions with fewer parentheses.

;;  To enter a Unicode character in a text editor in Debian 9,
;;  press Ctrl+Shift+U <code> space/enter.
;;
;;  <code>  character   description
;;  3bb 	λ       	lambda
;;
;;  We do not use Unicode Arrows because they are indistinguishable in VSCode 14px font.
;;
;;  2192	→       	rightward arrow
;;  21d2	⇒       	rightward double arrow
;;
;;  Examples:
;;  All of these mean the same thing:
;;
;;  	(lambda (a b c) (+ a b c))
;;
;;  	λ a b c -> (+ a b c)
;;
;;  	λ a b c => + a b c

(define-syntax-parser λ
    #:datum-literals (-> =>)
    [(_ Param ... -> Body ...)
        #'(lambda (Param ...) Body ...)]
    [(_ Param ... => Body ...)
        #'(lambda (Param ...) (Body ...))]
)

;;  --------------------    Date.
;;  See also racket/date.

(define (format-date/iso x)
    (define offset (date-time-zone-offset x))
    (define-values (offset-hour offset-m) (quotient/remainder (abs offset) 3600))
    (define offset-minute (quotient offset-m 60))
    (format "~a-~a-~aT~a:~a:~a~a~a:~a"
        (~r (date-year x) #:min-width 4 #:pad-string "0")
        (~r (date-month x) #:min-width 2 #:pad-string "0")
        (~r (date-day x) #:min-width 2 #:pad-string "0")
        (~r (date-hour x) #:min-width 2 #:pad-string "0")
        (~r (date-minute x) #:min-width 2 #:pad-string "0")
        (~r (date-second x) #:min-width 2 #:pad-string "0")
        (if (>= offset 0) '+ '-)
        (~r offset-hour #:min-width 2 #:pad-string "0")
        (~r offset-minute #:min-width 2 #:pad-string "0")
    ))

;;  --------------------	Grouping/Partitioning.

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
