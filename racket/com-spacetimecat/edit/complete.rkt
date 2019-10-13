#lang s-exp "lang.rkt"

(require
    racket/file
    "../racket/path.rkt"
)

(provide
    (struct-out Match)
    match-paths
    order-strings
    find-completions
    find-paths-for-quick-open
)

;;  Important: The "indexes/ascending" field contains an _ascending_ list of indexes.

(struct Match (string succeed? indexes/ascending score) #:prefab)

(define (Match-fail? m) (not (Match-succeed? m)))

;;  The scoring system is similar to that of some arcade games:
;;  There is a bonus multiplier for combos.
;;
;;  s, t are strings: s is user input; t is a choice.
;;  i, j are zero-based positions: i is for s; j is for t.
;;  m, n are exclusive upper bounds for positions.

(define/contract (match-string s t)
    (-> string?
        string?
        Match?)
    (define m (string-length s))
    (define n (string-length t))
    (define initial-bonus 1)
    (define (loop indexes score bonus i j)
        (if (or (>= i m) (>= j n))
            (Match t (>= i m) (reverse indexes) score)
            (if (equal? (string-ref s i) (string-ref t j))
                (loop
                    (cons j indexes)
                    (+ score bonus)
                    (* bonus 2)
                    (+ i 1)
                    (+ j 1))
                (loop
                    indexes
                    score
                    initial-bonus
                    i
                    (+ j 1))
            )
        )
    )
    (loop '() 0 initial-bonus 0 0)
)

;;  The returned list must not contain a Match that fails
;;  (whose "indexes/ascending" field contains an empty list).

(define/contract
    (match-paths
        #:in candidates
        #:according-to user-input
    )
    (-> #:in (listof path?)
        #:according-to string?
        (listof (cons/c path? Match?))
    )
    (define pairs (list-filter-map
        (λ path ->
            (define m (match-string user-input (path->string path)))
            (if (Match-fail? m) #f (cons path m))
        )
        candidates
    ))
    (list-sort pairs > #:key (λ p => Match-score (cdr p)) #:cache-keys? #f)
)

(define
    (order-strings
        #:in candidates
        #:according-to user-input
    )
    (define (order-of x)
        (cond
            [(equal? x user-input) 0]
            [(string_prefix? x user-input) 1]
            [else 2]
        ))
    (define (comes-before? x y)
        (if (< (order-of x) (order-of y))
            #t
            #f
        )
    )
    (sort candidates comes-before?)
)

(define
    (find-completions
        #:for input
        #:in strings
    )
    (order-strings #:in strings #:according-to input)
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
    (define-values (_base p-name _dir?) (split-path path))
    (define extension (string-downcase (path-get-extension/utf-8 path #:or "")))
    (if (symbol? p-name) ;; 'up or 'same
        #f
        (member extension wanted-file-extensions)
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

(define/contract (find-paths-for-quick-open)
    (-> (listof path?))
    ;(define orig-dir (find-system-path 'orig-dir))
    ;(define exec-file (find-system-path 'exec-file))
    ;(define main-collects-dir (find-system-path 'collects-dir))
    ;;  TODO: (current-library-collection-links) first
    (define dirs (list-remove-duplicates
        (list-map path-remove-trailing-slash
            (list-append
                (list (current-directory))
                (current-library-collection-paths)
            )
        )
    ))
    (list-concat-map traverse dirs)
)
