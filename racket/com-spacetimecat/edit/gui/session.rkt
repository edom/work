#lang s-exp "lang.rkt"

(provide
    buffer<%>
    session<%>
)

(define buffer<%> (interface ()
    (compute-word-completions-for   (->m string? (listof string?)))
    (get-word-near-cursor           (->m string?))
))

;;  A session functions as the hub in a hub-and-spokes network.
;;  Behavior, interaction, logic, hub, dispatch, event, handler, listener,
;;  bus (event bus), control, orchestrator, brain, mind, director, organizer.
(define session<%> (interface ()
    (get-current-buffer             (->m (is-a?/c buffer<%>)))
    (get-default-eval-namespace     (->m namespace?))
    (complete-word                  (->m string? any/c))
    (complete-word-near-cursor      (->m any/c))
))
