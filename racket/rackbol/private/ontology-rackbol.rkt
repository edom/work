#lang racket

(require "ontology.rkt")

(CLASS Machine
    [PROPERTY name]
    [PROPERTY address]
)
(CLASS Storage
    [PROPERTY type]
    [PROPERTY name]
)
(CLASS Procedure
    [PROPERTY name]
    [PROPERTY input]
    [PROPERTY output]
)

;;  Example.

(GET name OF [NEW Machine WITH (name the_machine) (color black)])
