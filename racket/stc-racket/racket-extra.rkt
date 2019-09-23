#lang racket

;;  --------------------	Racket syntax enhancements / language extensions.

(require (for-syntax syntax/parse))

(provide define-syntax-parse)

(define-syntax-rule (define-syntax-parse Name Arg ...)
    [define-syntax (Name stx)
        (syntax-parse stx Arg ...)
    ])

;;  --------------------    LET-like forms with fewer parentheses.

(provide LET)
(provide LET*)
(provide LETREC)
(provide LETREC*)
(provide with-environment)

(define-syntax-parse GLET
    #:datum-literals (IN)
    [(_ Let (~seq Name Expr) ... IN Body ...)
        #'(Let ((Name Expr) ...) Body ...)])

(define-syntax-rule (LET Arg ...) (GLET let Arg ...))
(define-syntax-rule (LET* Arg ...) (GLET let* Arg ...))
(define-syntax-rule (LETREC Arg ...) (GLET letrec Arg ...))
(define-syntax-rule (LETREC* Arg ...) (GLET letrec* Arg ...))

;;  This provides let-like syntax for parameterize-ing current-environment-variables.
;;  Keys are automatically quoted.

(define-syntax-rule (with-environment ((Key Value) ...) Thunk ...)
    (let ((env (make-environment-variables)))
        (environment-variables-set! env
            (string->bytes/utf-8 (->string 'Key))
            (string->bytes/utf-8 (->string Value))
        ) ...
        (parameterize ((current-environment-variables env)) Thunk ...)))

;;  --------------------    Destructuring bind.

(provide deconstruct)

;;  Create local variables that refer to the object properties with the same name.
;;  "Getter" is usually hash-ref or alist-ref.
;;  See also Racket "match".
;;  See also Common Lisp "destructuring-bind".
;;  See also TypeScript destructuring assignment.
;;  The advantage of "deconstruct" over "match"
;;  is that "deconstruct" requires typing the key once
;;  whereas "match" requires typing the key twice.
;;  Compare:
;;  (deconstruct h WITH hash-ref TO (k1 k2 k3))
;;  with:
;;  (match h (hash-table ('k1 k1) ('k2 k2) ('k3 k3)))

(define-syntax-parse deconstruct
    #:datum-literals (WITH FROM TO)
    [(deconstruct Table WITH Getter TO (Var ...))
        #'(define-values (Var ...) (values (Getter Table 'Var) ...))]
    [(deconstruct Table WITH Getter FROM (Index ...) TO (Var ...))
        #'(define-values (Var ...) (values (Getter Table Index) ...))])

;;  --------------------    Conversion to string.

(provide ->string)

;;  Convert any Racket value to string.
;;  (Should we use "write" instead?)

(define (->string x)
    (cond   ((string? x) x)
            ((number? x) (number->string x))
            ((symbol? x) (symbol->string x))
            (else (error "->string" x)) ))

;;  --------------------	Lambda expressions with less parentheses.

(provide λ)

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

(define-syntax-parse λ
    #:datum-literals (-> =>)
    [(_ Param ... -> Body ...)
        #'(lambda (Param ...) Body ...)]
    [(_ Param ... => Body ...)
        #'(lambda (Param ...) (Body ...))]
)

;;  --------------------	Grouping/Partitioning.

(provide GROUP)

(require racket/list)

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

(define-syntax-parse GROUP
    [(_ Source:Group_Source By:Group_By)
        #'(my_group By.Key_Func Source.Source)]
)

(define (my_group Key_Func Iterable)
	(define Groups (make-hash))
    ;;  Do we need in-list or in-vector?
    ;;  Do they improve performance?
	(for ([Elem Iterable])
    	(define key (Key_Func Elem))
    	(hash-update! Groups key (λ Old => cons Elem Old) '()))
	Groups
)
