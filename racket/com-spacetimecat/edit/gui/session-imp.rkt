#lang s-exp "lang.rkt"

(require
    "frame.rkt"
    "option.rkt"
    "session.rkt"
)

(provide
    session%
)

;;  2019-10-22:
;;  It seems that this centralization of behavior should reduce complexity
;;  because this centralization should make the programmers jump around files less.
(define session% (class* object% (session<%>)
    (init-field [default-eval-namespace (current-namespace)])
    (field [frame (new editor-frame% [session this] [width 1024] [height 768])]
           [outline-view (get-field outline-view frame)]
           [main-editor-area (get-field main-editor-area frame)]
           [status-indicator (get-field status-indicator frame)])
    (super-new)

    (define         -default-eval-namespace         default-eval-namespace)
    (define/public  (get-default-eval-namespace)    -default-eval-namespace)
    (define/public  (set-default-eval-namespace ns) (set! -default-eval-namespace ns))

    ;;  Commands.

    ;;  TODO: Replace define/forward with defcbc.
    ;;  defcbcs stands for define-current-buffer-commands.
    (define-syntax-rule (defcbcs [cmd arg ...] ...)
        (begin
            (define/public (cmd arg ...)
                (send (get-current-buffer) cmd arg ...))
            ...))

    (defcbcs
        (open-file path)
        (open-target path position)
        (save-file)
    )

    (define/forward     (evaluate-current-buffer) main-editor-area)
    (define/forward     (focus-on-command-input) main-editor-area)
    (define/forward     (focus-on-main-editor) main-editor-area)
    ;;  Is this the right way to request the application to quit?
    (define/public      (quit) (send frame on-exit))
    (define/public      (set-status-message str) (send status-indicator set-label str))

    (define/forward     (get-current-buffer) main-editor-area)

    ;;  Reactions.

    (define/public (before-open-file buf path) (send frame set-label (path->string path)))
    (define/public (after-open-file buf path) (send main-editor-area after-open-file buf path))
    (define/public (after-outline-change buf old new) (send outline-view set-outline new))

    ;;  Dialogs.

    (define session this)
    (define quick-open-dialog (new (class path-option-dialog%
        (super-new [label "Quick Open File"] [parent frame])
        (define/override (after-submit paths) (send session open-files paths)))))
    (define/public (quick-open-file)
        (send* quick-open-dialog [set-query ""] [show #t]))

    (define word-completion-dialog
        (new (class combo-option-dialog%
            (super-new [label "Choose Word"])
            (define/override (get-option-list-class)
                (class combo-option-list%
                    (super-new)
                    (define/override (compute-options-for query)
                        (send (get-current-buffer) compute-word-completions-for query))
                ))
            (define/override (after-submit answers)
                (let/ec return
                    (unless (= 1 (list-length answers)) (return #f))
                    (define buffer (get-current-buffer))
                    (send buffer delete-word-backwards)
                    (define answer (car answers))
                    (send buffer insert-string answer)
                    #t
                ))
            )))

    (define/public (complete-word-near-cursor)
        (complete-word (send (get-current-buffer) get-word-near-cursor)))

    (define/public (complete-word word)
        (define dialog word-completion-dialog)
        (define buffer (get-current-buffer))
        (define word (send buffer get-word-near-cursor))
        (send* dialog [remove-all-options] [set-query word])
        (send dialog show #t))

    ;;  TODO: Editor tabs, multiple buffers.

    (define/public (open-files paths)
        (for-each (λ path -> (open-file path)) paths)
        #t)

    (define/public (open-file/ask)
        ;; TODO: dir should be the directory of the current file.
        (define dir #f)
        (define path (get-file #f this dir))
        (when path (open-file path)))

    ;;  Initialization.

    (send frame show #t)
))
