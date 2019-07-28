#lang racket

;;  See also "share/pkgs/scribble-lib/scribble/run.rkt" in the Racket installation directory.

(require scribble/render)

(define (load-scribble-part path)
    (dynamic-require path 'doc)
)

(define (load-xref mod+id)
    (define get-xref (dynamic-require (car mod+id) (cdr mod+id)))
    (get-xref)
)

;;  These paths are relative to working directory, not to the path of the module being visited.
;;  https://stackoverflow.com/questions/42632854/dynamic-require-a-module-with-respect-to-the-current-module-path-in-racket-or-h

(define output-dir "_out")
(define xrefs (list (cons 'setup/xref 'load-collections-xref)))
(define paths (list "doc/main.scrbl"))
(define parts (map load-scribble-part paths))

(render parts paths
    #:dest-dir output-dir
    #:quiet? #f
    #:xrefs (map load-xref xrefs)
)
