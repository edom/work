#lang s-exp "lang.rkt"

(require
    setup/dirs
)

(provide
    find-paths-for-quick-open
)

(define/contract (get-dirs-for-quick-open)
    (-> (listof path?))
    ;(define orig-dir (find-system-path 'orig-dir))
    ;(define exec-file (find-system-path 'exec-file))
    ;(define main-collects-dir (find-system-path 'collects-dir))
    ;;  TODO: (current-library-collection-links) before (current-library-collection-paths)?
    (define racket-src-dir (string->path "/junk/racket-src"))
    (list-remove-duplicates
        (list-map path-remove-trailing-slash
            `(
                ,(current-directory)
                ,@(current-library-collection-paths)
                ,(find-user-pkgs-dir)
                ,(find-pkgs-dir)
                ,racket-src-dir
            ))))

(define/contract (find-paths-for-quick-open)
    (-> (listof path?))
    (list-concat-map traverse (get-dirs-for-quick-open)))

(define ignored-directory-names (list-map symbol->string '(
    |.git|
)))

(define wanted-file-extensions (list-map
    (λ s -> (string-append "." (symbol->string s)))
    '(
        rkt scrbl scm
        c cc cxx cpp c++
        h hh hxx hpp h++
        md txt org rst tex
        css js ts
    )))

(define (want-dir? path)        (not (list-member path ignored-directory-names)))
(define (want-extension? str)   (list-member (string-downcase str) wanted-file-extensions))

(define (want-file? path)
    (define-values (_base name _dir?) (split-path path))
    (and (path? name) ;; otherwise name may be 'up or 'same
         (want-extension? (path-get-extension/utf-8 path #:or ""))))

;;  Recursively list the files in dir.
;;  If dir is not a directory, this returns an empty list.
(define/contract (traverse dir)
    (-> path? (listof path?))
    (let/ec return
        (unless         (directory-exists? dir)     (return '()))
        (define         children                    (directory-list dir #:build? #t))
        (define-values  (child-dirs child-files)    (list-partition directory-exists? children))
        (define         wanted-dirs                 (list-filter want-dir? child-dirs))
        (define         wanted-files                (list-filter want-file? child-files))
        (list-append    wanted-files                (list-concat-map traverse wanted-dirs))))
