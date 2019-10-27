#lang s-exp "lang.rkt"

(require syntax/strip-context)

(current-namespace (make-base-namespace))

;;  2019-10-24: How do we specify the path for the module foo,
;;  to redirect (require "some-path.rkt") to (require 'foo)?

(eval-syntax
    (namespace-syntax-introduce
        (strip-context #'(module foo racket/base
            (displayln 'foo)
            (provide bar)
            (define bar 100)))))

(eval '(begin
    (require 'foo)
    bar))
