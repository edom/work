#lang racket

(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))
(require parser-tools/yacc)

;;  Separation of concerns:
;;  Do not interpret the lexemes while lexing,
;;  but return the lexemes as they are, as strings.

(require (for-syntax racket/syntax))
(define-syntax (make-token stx)
    (with-syntax* ( ((_ Kind) stx)
                    (token-Kind (format-id stx "token-~a" #'Kind)) )
        #'(token-Kind lexeme)
    )
)

(define-tokens my-tokens (EOF STARS HASHPLUS BACKSLASH MINUS COLON LF WHITE0 WHITE1 ANY))

(define lex (lexer-src-pos
    ((eof)              (make-token EOF))
    (#\linefeed         (make-token LF))
    (#\-                (make-token MINUS))
    (#\:                (make-token COLON))
    (#\\                (make-token BACKSLASH))
    ("#+"               (make-token HASHPLUS))
    ((:+ #\*)           (make-token STARS))
    ;((:+ whitespace)    (make-token WHITE1))
    ((:* whitespace)    (make-token WHITE0)) ;; What is the difference between whitespace and blank?
    (any-char           (make-token ANY))
))

(define (report-error tok-ok? tok-name tok-value start-pos end-pos)
    (printf "error ~v ~v ~v ~v ~v~n" tok-ok? tok-name tok-value start-pos end-pos)
    (printf "Parse error at line ~v column ~v~n" (position-line start-pos) (position-col start-pos))
)

(define parse (parser
    (tokens my-tokens)
    (start start)
    (end EOF)
    (error report-error)
    (src-pos)
    (grammar
        (start
            (()                 '())
            ((content start)    (cons $1 $2))
        )
        (content
            ((property)     $1)
            ((section)      $1)
            (($body)        `(body ,$1))
        )

        (property
            ((HASHPLUS $pname COLON WHITE0 $title)     `(property $1 $2 $3 $4 $5))
        )
        ($pname
            ((ANY)              $1)
            ((ANY $pname)       (string-append $1 $2))
        )

        (section
            ((STARS WHITE0 $title)  `(section ,$1 ,$2 ,$3))
        )

        ;;  Anything until LF.
        ($title
            ((LF)                   $1)
            ((ANY $title)           (string-append $1 $2))
            ((MINUS $title)         (string-append $1 $2))
            ((COLON $title)         (string-append $1 $2))
            ((WHITE0 $title)        (string-append $1 $2))
        )

        ($body
            (($atom)                $1)
            (($atom $body)          (string-append $1 $2))
        )
        ;;  This sucks.
        ($atom
            ((LF)           $1)
            ((WHITE0)       $1)
            ((ANY)          $1)
        )
    )
))

(define str "
* A
** B
*** CDE
FGH.
")

(parameterize ((port-count-lines-enabled #t))
    ;(call-with-input-string str (lambda (port)
    (call-with-input-file "../edom.github.io/other.org" (lambda (port)
        (for ((i 5))
            (printf "~v~n" (lex port))
        )
    ))

    ;(call-with-input-string str (lambda (port)
    (call-with-input-file "../edom.github.io/other.org" (lambda (port)
        (pretty-print
            (parse (lambda () (lex port)))
        )
    ))
)
