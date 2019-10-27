#lang s-exp "lang.rkt"

(require
    "../eval.rkt"
    "option.rkt"
    "session.rkt"
    "style.rkt"
)

(provide
    buffer%
)

;;  Each instance of this class is intended to correspond to one open file.
;;  1 buffer ~ 1 tab ~ 1 namespace ~ 1 file path.
;;
;;  A buffer% can be thought of as a text% + a namespace.

(define buffer% (class* object% (buffer<%>) (super-new)
    (init-field session)
    (define text (new my-text% [event-handler (match-lambda
        [`(complete-word ,word) (send session complete-word word)]
        [_ (void)])]))

    ;;  Overridables, event listeners, hooks.

    (define/public (get-default-eval-namespace) (send session get-default-eval-namespace))

    ;;  Internals.

    (define/public  (internal-get-text%-instance)   text)

    ;;  Naming this "get-text" would be too pretentious, because we don't use the fancy snip features of text%.
    ;;  We only handle character strings, a very simple linear kind of text.
    (define/public  (get-string)                    (send text get-text))

    ;;  Editing commands.
    ;;  TODO: Mark buffer as dirty.

    (define/public  (append str)            (send text insert str (send text last-position)))
    (define/public  (insert-char c)         (send text insert (string c)))
    (define/public  (insert-string s)       (send text insert s))
    (define/public  (handle-backspace-key)  (send text delete 'start 'back))
    (define/public  (handle-delete-key)     (send text delete 'start (send text get-end-position)))
    (define/forward (delete-word-backwards) text)

    ;;  Navigation commands.

    (define/public  (move-cursor dir)                   (send text move-position dir))
    (define/public  (move-to-start-of-next-word)        (TODO))
    (define/public  (move-to-previous-start-of-word)    (TODO))
    (define/public  (move-to-next-end-of-word)          (TODO))

    ;;  Namespace, parsing, outline, eval, completion, and cross-references.

    (define namespace #f)
    (define/public (get-eval-namespace) (if namespace namespace (get-default-eval-namespace)))
    (define/public (set-eval-namespace ns) (set! namespace ns))

    (define-property outline #f
        #:after-change (old new -> (send session after-outline-change this old new)))

    ;;  Beware: The REPL runs on the GUI thread, so the GUI will hang until eval returns.
    (define/public (evaluate)
        (define input-str (get-string))
        (define output-str (eval/string input-str (get-eval-namespace)))
        (send this append (string-append "\n\n" output-str)))

    (define/forward (get-word-near-cursor) text)

    (define/public (compute-word-completions-for word)
        (parameterize [(current-namespace (get-eval-namespace))]
            (define sym-strs (map symbol->string (namespace-mapped-symbols)))
            (define completions (find-completions #:for word #:in sym-strs))
            (list-take-at-most 16 #:from completions)))

    ;;  open = load + parse + fire events
    ;;
    ;;  2019-10-16: Design Question:
    ;;  Should open-path accept relative paths?
    ;;  If yes, what should it be relative to?
    ;;  The working directory? The project directory?

    ;;  Return #f if the current buffer is not associated with a file.
    (define/public (get-file-path)
        (define pathy (send text get-filename))
        (cond   [(not pathy)    #f]
                [(path? pathy)  pathy]
                [else           (string->path pathy)]))

    ;;  If this buffer already represents the path, do nothing.
    ;;  TODO: 2019-10-18:
    ;;  Watch the file system.
    ;;  Record file mtime on load.
    ;;  Compare mtime periodically or on save.
    ;;  Someone else may have changed the file.
    ;;  But can we assume that the underlying filesystem always has mtime?
    (define/public (open-file path)
        (define current-path (get-file-path))
        (unless (equal? current-path path)
            (send session before-open-file this path)
            (send text load-file path)
            (send this set-outline (compute-file-outline path))
            (send this set-eval-namespace (make-namespace-from-file path))
            (send session after-open-file this path)
            ))

    (define/public (open-target target-path position)
        (send this open-file target-path)
        (send this set-position position position))

    (define/forward (set-position begin end) text)
    (define/forward (save-file) text)
))

(define my-text% (class text%
    (init [event-handler (λ e -> (void))])
    (super-new)
    (define text this)

    (define handle-event event-handler)

    (define/override (on-event e)
        (define admin (send text get-admin))
        (if (and admin (send e button-down? 'right))
            (send admin popup-menu (make-context-menu) (send e get-x) (send e get-y))
            (super on-event e)))

    ;;  TODO: Ask to save if content is dirty.
    ;;  But what should we do if the user presses "Cancel"?
    ;;  Do nothing? Throw exception? Return #f?
    (define/override (load-file path)
        (unless (path? path) (error 'load-file "not a path: ~s" path))
        (super load-file path 'text))

    (define/public (delete-word-backwards)
        (define start (send this get-start-position))
        (let/ec return
            (when (<= start 0) (return))
            (define char (send this get-character (- start 1)))
            (when (char-breaks-word? char) (return))
            (send this delete 'start 'back)
            (delete-word-backwards)))

    (define/public (get-word-near-cursor)
        (define-values (start end) (extend-selection-to-nearest-word-boundaries))
        (send this get-text start end))

    (define/public (extend-selection-to-nearest-word-boundaries)
        (define start (send this get-start-position))
        (define end (send this get-end-position))
        (define min-start 0)
        (define max-end (send this last-position))
        (loop #:to-break-call break
            (when (<= start min-start) (break))
            (define char (send this get-character (- start 1)))
            (when (char-breaks-word? char) (break))
            (set! start (- start 1)))
        (loop #:to-break-call break
            (when (>= end max-end) (break))
            (define char (send this get-character end))
            (when (char-breaks-word? char) (break))
            (set! end (+ end 1)))
        (values start end))

    ;;  Menu.
    (define (make-context-menu)
        (define word (get-word-near-cursor))
        (define menu (new popup-menu%))
        (define-syntax-rule (f arg ...) (~a arg ... #:separator " "))
        (new menu-item% [parent menu] [label (f "Word near cursor:" word)] [callback (λ r e -> (TODO))])
        (new menu-item% [parent menu] [label (f "Complete" word)] [callback (λ r e -> (handle-event `(complete-word ,word)))])
        (new menu-item% [parent menu] [label (f "TODO: Define" word)] [callback (λ r e -> (TODO))])
        (new menu-item% [parent menu] [label "TODO: Open <module>"] [callback (λ r e -> (TODO))])
        (new menu-item% [parent menu] [label (f "TODO: Show documentation for" word)] [callback (λ r e -> (TODO))])
        (new menu-item% [parent menu] [label "TODO: Find what uses <word/module>"] [callback (λ r e -> (TODO))])
        (new menu-item% [parent menu] [label "TODO: Find what is used by <word/module>"] [callback (λ r e -> (TODO))])
        (new menu-item% [parent menu] [label "TODO: Show <word/module> dependencies/uses/used-by"] [callback (λ r e -> (TODO))])
        menu)

    ;;  Styles.
    (define/override (default-style-name) "Code")
    (initialize-styles this)
))

(define word-breaking-chars '(
    #\nul #\space #\tab #\newline #\return
    #\( #\) #\[ #\] #\{ #\}
))

(define (char-breaks-word? c)
    (member c word-breaking-chars))

(define-syntax-parser loop
    [   (_ #:to-break-call break body ...)
        #'(call/ec (λ break ->
            (define (Loop) body ... (Loop))
            (Loop)
        ))
    ])
