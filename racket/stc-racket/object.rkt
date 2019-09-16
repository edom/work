#lang racket

;;  This is an object system.
;;
;;  An object is a mutable property bag (more correctly a "property set").
;;  The order of properties is unimportant.
;;  A property occurs at most once.
;;
;;  Users must assume that all exported symbols are forms, not procedures.
;;
;;  Users must not depend on the fact that objects happen to be implemented with hash tables.

(provide make-object)
(provide make-object/quasiquote)
(provide object-get-property)
(provide object-set-property!)
(provide object-deconstruct)

(require (for-syntax syntax/parse))

;;  (make-object Key1 Value1 ... KeyN ValueN)
;;  (make-object/quasiquote Key1 Value1 ... KeyN ValueN)

(define-syntax (make-object stx)
    (syntax-parse stx
        [(_ (~seq Key Value) ...)
            #'(make-hash (list (cons Key Value) ...))]))

;;  The "/quasiquote" variant automatically quasiquotes Keys and Values.

(define-syntax (make-object/quasiquote stx)
    (syntax-parse stx
        [(_ (~seq Key Value) ...)
            #'(make-hash (list (cons `Key `Value) ...))]))

;;  (object-get-property Object Key)
;;  (object-set-property! Object Key)
;;
;;  Key must be a symbol.
;;
;;  Key must not begin with the dollar sign ("$").
;;  We reserve all such keys.

(define-syntax-rule (object-get-property Object Key) (hash-ref Object Key))
(define-syntax-rule (object-set-property! Object Key Value) (hash-set! Object Key Value))

;;  Example: (object-deconstruct my_point x y)

(define-syntax-rule (object-deconstruct Object Key ...)
    (begin (define Key (object-get-property Object 'Key)) ...))
