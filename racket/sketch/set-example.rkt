#lang racket

(require "set.rkt")

;;  We can make a #lang that decorates Racket's struct.

(define-struct-2 Employee (name age) #:prefab #:mutable)
(define-struct-2 Department (name) #:prefab #:mutable)

*struct-info-hash*
(get (Employee "Alice" 30) 'name)
(get (Employee "Alice" 30) 'buddy)
(get (vector 1 2 3) 0)
