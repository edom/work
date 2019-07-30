#lang racket

(require (prefix-in emacs: "read.rkt"))
(require (prefix-in emacs: "interpret.rkt"))

;;  DEBUG
#;
(emacs:with-input-from-file "/home/erik/work/etc/emacs.d/elpa/org-plus-contrib-20180910/org.el" (lambda ()
    (pretty-print (emacs:read-until-eof))
))

(define file (vector-ref (current-command-line-arguments) 0))
(emacs:load file)
