#lang s-exp "lang.rkt"

;;  This program writes a vim tags file to standard output.
;;  Usage:
;;  Run "racket -t <this-file> -m > tags"
;;  and then run "vim -l" in the directory containing the "tags" file.

(require
    syntax/modresolve
    "../racket/syntax.rkt"
)

(provide main)

;;  This is the list of modules to include in the output.
(define modules '(
    racket/gui
    racket/syntax
))

;;  2019-10-21: Should we traverse the dependencies with "module->imports"?
(define (module->+deps mod)
    (eprintf "Loading ~a...~n" mod)
    (namespace-require mod)
    (list mod))

(define (main)
    ;;  How do we get the line number and column?
    ;;  It seems that we have to expand-syntax-to-top-form
    ;;  the module ourselves and scrounge the define-values?
    (parameterize ([current-namespace (make-base-namespace)])
        (define mods (list-append-map module->+deps modules))
        (define namespaces (list-map module->namespace mods))
        (struct Site (id path) #:prefab)
        (define sites (list-append-map
            (λ ns ->
                (define syms (namespace-mapped-symbols ns))
                (list-append-map
                    (λ sym ->
                        (define id (namespace-syntax-introduce (datum->syntax #f sym) ns))
                        (define binding (identifier-binding id))
                        (match binding
                            [(list src-mpi src-id _ _ _ _ _)
                                (define path (resolved-module-path-name (module-path-index-resolve src-mpi)))
                                (match path
                                    [(? path?) (list (Site (symbol->string src-id) path))]
                                    [(? symbol?) '()]
                                )]
                            [_ '()])
                    )
                    syms))
            namespaces
        ))
        (define ordered-sites (sort sites string<? #:key Site-id))
        ;;  2019-10-21: Should we deduplicate the results?
        (list-for-each
            (match-lambda [(Site id path)
                (printf "~a\t~a\t/~a~n" id path (escape-ex-regex-str id))
            ])
            ordered-sites)))

;;  TODO
(define (escape-ex-regex-str str) str)
