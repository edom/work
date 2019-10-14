#lang s-exp "lang.rkt"

(require
    (prefix-in m: "markup.rkt")
    "list.rkt"
)

(provide
    quick-open-dialog%
)

(define/contract (get-dirs-for-quick-open)
    (-> (listof path?))
    ;(define orig-dir (find-system-path 'orig-dir))
    ;(define exec-file (find-system-path 'exec-file))
    ;(define main-collects-dir (find-system-path 'collects-dir))
    ;;  TODO: (current-library-collection-links) first
    ;;  TODO: Add the $RACKET/share/ directory?
    (define racket-src-dir (string->path "/junk/racket-src"))
    (list-remove-duplicates
        (list-map path-remove-trailing-slash
            (list-append
                (list (current-directory))
                (current-library-collection-paths)
                (list racket-src-dir)
            )
        )
    )
)

(define/contract (find-paths-for-quick-open)
    (-> (listof path?))
    (list-concat-map traverse (get-dirs-for-quick-open))
)

(define ignored-directory-names (map symbol->string '(
    |.git|
)))

(define wanted-file-extensions (map
    (λ s -> (string-append "." (symbol->string s)))
    '(
        rkt scrbl scm
        c cc cxx cpp c++
        h hh hxx hpp h++
        md txt org rst tex
        css js ts
    )))

(define (use-dir? path)
    (not (member path ignored-directory-names)))

(define (use-file? path)
    (define-values (_base name _dir?) (split-path path))
    (define extension (string-downcase (path-get-extension/utf-8 path #:or "")))
    (if (path? name)
        (member extension wanted-file-extensions)
        #f  ;; if name is 'up or 'same
    ))

(define (traverse dir) ;; -> directory (listof file)
    (define children (if (directory-exists? dir)
        (directory-list dir #:build? #t)
        '()
    ))
    (define-values (child-dirs child-files) (list-partition directory-exists? children))
    (define dirs (list-filter use-dir? child-dirs))
    (define files (list-filter use-file? child-files))
    (append files (list-concat-map traverse dirs))
)

(define quick-open-dialog% (class dialog%
    (super-new
        [label "Quick Open"]
        [width 640]
        [stretchable-height #t]
    )

    (define dialog this)

    (define top-bar (new horizontal-panel% [parent dialog]))

    (define text-field (new text-field%
        [label "Pattern"]
        [parent top-bar]
        [callback (λ c e ->
            (case (send e get-event-type)
                [(text-field) (on-change)]
                [(text-field-enter) (on-commit)]
            )
        )]
    ))

    (define open-button (new button%
        [label "Open"]
        [parent top-bar]
        [callback (λ b e -> (on-commit))]
    ))

    (define result-panel
        (new quick-open-choice-list%
            [parent dialog]
            [stretchable-height #t]
        ))

    (define/override (on-activate active?)
        (when active? (send text-field focus)))

    (define/public (clear)
        ;;  set-value does not call callback.
        (send text-field set-value "")
        (on-change)
    )

    (define (on-change)
        (with-container-sequence result-panel
            (define query (send text-field get-value))
            (send result-panel clear)
            (send dialog resize 640 (send dialog min-height))
            (when (non-empty-string? query)
                (update-result-for query)))
        (send dialog reflow-container)
    )

    (define (on-commit)
        (define children (send result-panel get-selected-children))
        (define paths (list-map (λ c => send c get-user-data) children))
        (for-each (λ p => open-file p) paths)
        (unless (list-empty? paths) (send dialog show #f))
    )

    (define/public (open-file path)
        (printf "Not overridden: open-file ~s~n" path))

    (define (update-result-for query)
        (define all-paths (find-paths-for-quick-open))
        (define pairs (match-paths #:in all-paths #:according-to query))
        (define limit 16)
        (define results (list-take-at-most limit #:from pairs))
        (define paths (list-map car results))
        (define matches (list-map cdr results))
        (define markups (list-map match->markup matches))
        (for ([path (in-list paths)] [markup (in-list markups)])
            (make-result-item path markup)
        )
        (send result-panel try-set-selected-indexes '(0))
    )

    (define (make-result-item path markup)
        (new quick-open-choice%
            [parent result-panel]
            [stretchable-height #f]
            [markup markup]
            [user-data path]
        )
    )

    (struct Span (begin end))
    (define empty-Span (Span 0 0))

    ;;  The input indexes must be ordered ascending.

    (define (group-adjacent-indexes indexes)    ;; : [[Index]]
        (define (loop span indexes)             ;; : [Index] -> [Index] -> [Span]
            (if (null? indexes)
                (list span)
                (let (
                        [begin (Span-begin span)]
                        [end (Span-end span)]
                        [ind (car indexes)]
                        [rest (cdr indexes)]
                    )
                    (if (= end ind)
                        (loop (Span begin (+ end 1)) rest)
                        (cons span (loop (Span ind (+ ind 1)) rest))
                    )
                )
            )
        )
        (if (null? indexes)
            (list empty-Span)
            (let (  [ind (car indexes)]
                    [rest (cdr indexes)]
                )
                (loop (Span ind (+ ind 1)) rest))))

    (define (match->markup the-match)
        (define string (Match-string the-match))
        (define highlight-indexes (Match-indexes/ascending the-match))
        ;;  This is easier to write but less efficient.
        ;;  However, it would be harder to write but more efficient to use span-subtract,
        ;;  which could be defined in terms of span-invert and span-intersect.
        (define normal-indexes (for/list [
                (i (in-range (string-length string)))
                #:when (not (member i highlight-indexes))
            ]
            i))
        (define highlight-spans (group-adjacent-indexes highlight-indexes))
        (define normal-spans (group-adjacent-indexes normal-indexes))
        (define (substr span) (substring string (Span-begin span) (Span-end span)))
        (define span-markup-pair
            (list-sort
                (list-append
                    (list-map (λ s => cons s (m:string (substr s))) normal-spans)
                    (list-map (λ s => cons s (m:bold (m:string (substr s)))) highlight-spans)
                )
                <
                #:key (λ a => Span-begin (car a))
                #:cache-keys? #f
            ))
        (m:hflow (list-map cdr span-markup-pair))
    )
))
