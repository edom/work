(module check "stc-racket.rkt"

    ;;  check free identifiers (but Racket already does this for modules...)

    ;;  List of symbols.

    (define unchecked-symbols '(
        #%app
    ))

    (define (_normalize pathlike)
        (cond
            ((path? pathlike)
                (simple-form-path pathlike)
            )
            ((string? pathlike)
                (_normalize (string->path pathlike))
            )
            (else (error "_normalize" pathlike))
        )
    )

    (define checked-directories
        (list-map _normalize (list
            (current-directory)
            ;;  Optional: Add directories here. Strings are OK.
        ))
    )

    ;;  Both paths should have been normalized with simple-form-path first.

    (define (path-prefix? a b)
        (list-prefix?
            (explode-path a)
            (explode-path b)
        )
    )

    (define (test)
        (printf "~v~n" (path-prefix? (string->path "/a") (string->path "/a")))
    )

    (define (identifier-should-be-checked? syntax)
        (define source (syntax-source syntax))
        (define is-unchecked-symbol (delay
            (list-member (syntax->datum syntax) unchecked-symbols))
        )
        (define in-checked-dir (delay
            (define complete-source (path->complete-path source))
            (ormap (lambda (dir) (path-prefix? dir complete-source)) checked-directories)
        ))
        (and
            (not (force is-unchecked-symbol))
            source
            (force in-checked-dir)
        )
    )

    ;;  identifiers-of : Syntax -> Listof Syntax

    (define (identifiers-of thing)
        (cond
            ((identifier? thing)
                (if (identifier-should-be-checked? thing)
                    (list thing)
                    empty-list
                )
            )
            ((syntax? thing)        (identifiers-of (syntax-e thing)))

            ;;  containers

            ((empty-list? thing)    empty-list)
            ((cons? thing)
                (let (
                        (x (car thing))
                        (y (cdr thing))
                    )
                    (list-append (identifiers-of x) (identifiers-of y))
                )
            )
            ((vector? thing)        (identifiers-of (vector->list thing)))
            ((hash? thing)          (identifiers-of (hash->list thing)))

            ;;  literals

            ((string? thing)        empty-list)
            ((symbol? thing)        empty-list)
            ((number? thing)        empty-list)
            ((boolean? thing)       empty-list)

            (else                   (error "unsupported syntax-e" thing))
        )
    )

    ;;  check : Syntax -> Void
    ;;
    ;;  Syntax should have been fully-expanded.
    ;;  https://docs.racket-lang.org/reference/Expanding_Top-Level_Forms.html

    (define (check syntax)
        (define (free-identifier? syntax)
            (define phase 0)
            (define top-level? #t)
            (and    (identifier? syntax)
                    (equal? #f (identifier-binding syntax phase top-level?))
            )
        )
        (define ids (identifiers-of syntax))
        (define free-ids (list-filter free-identifier? ids))
        (for ((id (in-list free-ids)))
            (printf "warning: free identifier: ~v~n" id)
        )
    )

    ;;  -------------------- test

    (require "type.rkt")

    ;;  https://docs.racket-lang.org/reference/Expanding_Top-Level_Forms.html

    (define (expand-1 syntax)
        (parameterize ((current-namespace (make-base-namespace)))
            (expand syntax)
        )
    )

    (define-module* main #f

        (define (test x)
            (printf "~v : ~v~n" x (apparent-type-of x))
        )

        (define (st1 s) (syntax-case s ()
            ((st1 a)
                #'(list 'a 'a)
            )
        ))

        (test 5)
        (test +)
        (test #'+)
        (test (list 1 2))
        (test #(1 2 3))
        (test `(,+ 1 2))
        (print (st1 #'(st1 a)))

        #|
        (parameterize ((read-accept-reader #t))
            (define source "test-check.rkt")
            (call-with-input-file source (lambda (port)
                (port-count-lines! port)

                (define syntax_1 (read-syntax source port))
                (define syntax_2 (expand-1 syntax_1))

                (display "------------------------------ read\n")
                (pretty-print (syntax->datum syntax_1))
                (display "------------------------------ expand\n")
                (pretty-print (syntax->datum syntax_2))
                (display "------------------------------ check\n")
                (check syntax_2)
                (display "------------------------------ decorate with types?\n")
                (pretty-print (Typed->datum (decorate syntax_2)))
            ))
        )
        |#
    )

)
