#lang s-exp "lang.rkt"

;;  Software maintenance assistant.
;;
;;  This module is intended for user interaction with REPL,
;;  not for require as a library.
;;
;;  TODO: Extract a library from this file.

(require
    racket/match
    syntax/modcode
    syntax/moddep
    "../racket/string.rkt"
)

(provide

    where-is
    analyze-dependencies

)

;;  Describe where an identifier comes from.
;;
;;  Example:
;;
;;      (where-is define)

(define-syntax where-is
    (syntax-rules ()
        [   (_ id)
            (-where-is #'id)
        ]
        [   (_ id phase)
            (-where-is #'id phase)
        ]
    ))

(define (-where-is id-stx [phase 0])
    (define id-sym (syntax->datum id-stx))
    (define message
        (match (identifier-binding id-stx phase #t)
            [   #f
                (~a "In phase " phase " of this module, " id-sym " is not defined.\n")
            ]
            [   'lexical
                (~a "In phase " phase " of this module, " id-sym " is a local variable.\n")
            ]
            [   (list source-id)
                (~a "In phase " phase " of this module, " id-sym " is a top-level variable.\n")
            ]
            [   (list source-mod source-id nominal-source-mod nominal-source-id
                    source-phase import-phase nominal-export-phase
                )
                (define src-mod (module-path-index-resolve source-mod #f))
                (define nom-src-mod (module-path-index-resolve nominal-source-mod #f))
                (~a "Where is " (~v id-sym) " defined?\n"
                    "\n"
                    "    It is defined as " (~v source-id) " in phase " source-phase " in " src-mod ".\n"
                    "\n"
                    "How does it come into phase " phase " of this module?\n"
                    "\n"
                    "    It is imported, with import phase offset " import-phase ",\n"
                    "    through " nom-src-mod " that exports it as " (~v nominal-source-id)
                        " at phase " nominal-export-phase ".\n"
                )
            ]
        )
    )
    (display message)
)

(define-syntax-rule (analyze-dependencies require-spec)
    (-analyze-dependencies 'require-spec)
)

(define (-analyze-dependencies require-spec)
    (define mod-path
        (if (string? require-spec)
            `(file ,require-spec)
            require-spec
        ))
    (define main-collection
        (path->string
            ;;  (path->complete-path (find-system-path 'collects-dir))
            (list-ref (find-library-collection-paths) 1)
        ))
    (define include-main-collection? #f)
    (define (show indent path require-mode phase)
        (define file-path
            (match path
                [(? path? _) (path->string path)]
                [(list 'submod path _) (path->string path)]
            ))
        (define (is-in-main-collection?)
            (string_prefix? file-path main-collection))
        (define should-show?
            (or include-main-collection?
                (not (is-in-main-collection?))
            ))
        (when should-show?
            (printf "~a~a~n" indent (~a path require-mode phase #:separator " "))
        ))
    ;;  Problem: 2019-10-11: This is slow.
    (show-import-tree mod-path #:dag? #t #:show show)
)
