#lang s-exp "lang.rkt"

;;  Forms with fewer parentheses.

(provide

    Let Let* Letrec Letrec*
    LET LET* LETREC LETREC*

    λ   ;;  This conflicts with Racket's λ.

    !=
    not-equal?

)

(define-syntax-parser _Glet
    [(_ Let (~seq Name:id Expr:expr) ... #:in Body ...)
        #'(Let ((Name Expr) ...) Body ...)])

(define-syntax-rule (Let Arg ...) (_Glet let Arg ...))
(define-syntax-rule (Let* Arg ...) (_Glet let* Arg ...))
(define-syntax-rule (Letrec Arg ...) (_Glet letrec Arg ...))
(define-syntax-rule (Letrec* Arg ...) (_Glet letrec* Arg ...))

;;  --------------------    LET-like forms with fewer parentheses.

(define-syntax-parser _GLET
    #:datum-literals (IN)
    [(_ Let (~seq Name Expr) ... IN Body ...)
        #'(Let ((Name Expr) ...) Body ...)])

(define-syntax-rule (LET Arg ...) (_GLET let Arg ...))
(define-syntax-rule (LET* Arg ...) (_GLET let* Arg ...))
(define-syntax-rule (LETREC Arg ...) (_GLET letrec Arg ...))
(define-syntax-rule (LETREC* Arg ...) (_GLET letrec* Arg ...))

;;  --------------------    Lambda expressions with fewer parentheses.

;;  To enter a Unicode character in a text editor in Debian 9,
;;  press Ctrl+Shift+U <code> space/enter.
;;
;;  <code>  character   description
;;  3bb     λ           lambda
;;
;;  We do not use Unicode Arrows because they are indistinguishable in VSCode 14px font.
;;
;;  2192    →           rightward arrow
;;  21d2    ⇒           rightward double arrow
;;
;;  Examples:
;;  All of these mean the same thing:
;;
;;      (lambda (a b c) (+ a b c))
;;
;;      λ a b c -> (+ a b c)
;;
;;      λ a b c => + a b c
;;
;;  2019-10-20: Problem:
;;  Mistaking => for -> causes extremely unhelpful runtime error messages.
;;  There are two solutions: Add type-checking, or remove this form.

(define-syntax-parser λ
    #:datum-literals (-> =>)
    [(_ Param ... -> ~! Body1 Body ...)
        #'(lambda (Param ...) Body1 Body ...)]
    [(_ Param ... => ~! Body1 Body ...)
        #'(lambda (Param ...) (Body1 Body ...))]
)

(define (!= a b)                (not (= a b)))
(define (not-equal? a b)        (not (equal? a b)))
