#lang racket/base

(require
    racklog
)

;;  The database to be queried.

(require "family.rkt")

(%find-all [P C] [father P C])
(%find-all [P C] [mother P C])
(%find-all [F M C] (%and [father F C] [mother M C]))
