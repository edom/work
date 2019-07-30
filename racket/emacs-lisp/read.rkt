#lang racket

(require (only-in syntax/readerr raise-read-error))

(require "string.rkt")

(provide current-source)
(provide (rename-out (my-read read)))
(provide read-until-eof)
(provide (rename-out (my-with-input-from-file with-input-from-file)))

;;  Path to the file being read, for error reporting only.
(define current-source (make-parameter #f))

;;  This function will be passed to make-readtable.
(define (handle char port source line column position)

    ;;  --------------------    Helper functions.

    (define (error fmt . args)
        (define msg (apply format fmt args))
        ;;  Quagmire: "raise-read-error" does not print these if
        ;;  we get here by "read" instead of by "read-syntax".
        (printf "Error: While reading ~a: In line ~a column ~a:~n" source line column)
        (raise-read-error msg source line column position #f))

    ;;  Return a char, not an integer.
    (define (read-char-raw) (read-char port))

    ;;  This is called after ";" is read.
    (define (read-comment)
        ;;  The line terminator is discarded.
        (define line (read-line port 'any))
        (if (eof-object? line)
            '(#%comment "")
            `(#%comment ,line)))

    ;;  Characters that must be escaped in a character literal, such as "?\(" and "?\)".
    (define (common-special-char? c)
        (member c '(#\( #\) #\[ #\] #\' #\; #\\) char=?))

    ;;  This is called after "?" is read.
    ;;  Return an integer, not a char.
    (define (read-char-literal)
        (define u0 (read-char-raw))
        (cond
            ((char=? #\\ u0) (escape))
            ((not (common-special-char? u0)) (char->integer u0))
            (else (error "Unexpected ?character-literal: ~a" u0))))

    ;;  This is called after the opening quote is read.
    ;;  Read string until the closing quote.
    ;;  See also "racket/src/expander/read/string.rkt" in the "racket" Git repository.
    (define (read-string-literal)
        (define cap 1024)
        (define str (make-String cap))
        (define len 0)
        (define (app! code)
            (when (>= len cap)
                (define new-cap (* 2 cap))
                (define new-str (make-String new-cap))
                (String-copy! new-str 0 str)
                (set! str new-str)
                (set! cap new-cap))
            (String-set! str len code)
            (set! len (+ len 1)))
        (define (loop)
            (define char (read-char-raw))
            (when (eof-object? char) (error "Unexpected end of file while reading string literal"))
            (case char
                ((#\")
                    (String-substring str 0 len))
                ((#\\)
                    (define e (escape))
                    (when e (app! e))
                    (loop))
                (else
                    (app! (char->integer char))
                    (loop))))
        (loop))

    (define (ascii-upcase n)
        (if (and (<= 97 n) (<= n 122)) ;; turn lowercase ASCII letter...
                (- n 32) ;; ...to uppercase ASCII letter
                n))

    ;; Translate X to Control-X, etc.
    ;; Translate Meta-X to Meta-Control-X, etc.
    (define (key->control+key code)
        (define my-meta-mask (bitwise-and code meta-mask))
        (define graphic-code (bitwise-and code (- meta-mask 1)))
        (define upcode (ascii-upcase graphic-code))
        ;; The number 64 is so that C-A is 1, C-B is 2, C-C is 3, etc.
        (define control-code (- upcode 64))
        (bitwise-ior my-meta-mask control-code))

    ;;  This is called after the escape character (backslash) is read.
    ;;  Backslash escape sequences.
    ;;  https://www.gnu.org/software/emacs/manual/html_node/elisp/Character-Type.html
    (define (escape)
        (define u0 (read-char-raw))
        (when (eof-object? u0)
            (error "Unexpected end of file while reading character escape sequence"))
        (case u0
            ;;  The only case that does not return a char.
            ((#\newline) #f)
            ;;  Other escape sequences.
            ((#\C)
                (define u1 (read-char-raw))
                (unless (char=? u1 #\-) (error "Expecting a hyphen after \\C"))
                (define u2 (read-char-literal))
                (key->control+key u2))
            ((#\^)
                (define u1 (read-char-literal))
                (key->control+key u1))
            ((#\M)
                ;;  TODO Set bit 7 in a string, and set bit 27 in a character literal (integer),
                ;;  for backward compatibility.
                ;;  https://www.gnu.org/software/emacs/manual/html_node/elisp/Meta_002dChar-Syntax.html
                (define u1 (read-char-raw))
                (unless (char=? u1 #\-) (error "Expecting a hyphen after \\M"))
                (define u2 (read-char-literal)) ;; Handle stuffs like \M-\C-A.
                (bitwise-ior meta-mask u2))
            ;;  https://www.gnu.org/software/emacs/manual/html_node/elisp/Basic-Char-Syntax.html
            ((#\a) 7)
            ((#\b) 8)
            ((#\t) 9)
            ((#\n) 10)
            ((#\v) 11)
            ((#\f) 12)
            ((#\r) 13)
            ((#\e) 27)
            ((#\s) 32)
            ((#\\) 92)
            ((#\d) 127)
            ;;  Character code in octal.
            ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
                (define u1 (read-char-raw))
                (define u2 (read-char-raw))
                (string->number (string u0 u1 u2) 8))
            ;;  Characters that escape themselves.
            ((#\" #\space) (char->integer u0))
            (else (cond
                ((common-special-char? u0) (char->integer u0))
                (else (error "escape not implemented: ~a" u0)))
            )))

    ;;  --------------------    The actual handler.

    (case char
        ((#\;) (read-comment))
        ((#\?) (read-char-literal))
        ((#\") (read-string-literal))
    ))

(define my-readtable (make-readtable #f
    ;;  We parse comments to enable later parsing of buffer-local variables
    ;;  such as "; -*- lexical-binding: t; -*-".
    #\; 'terminating-macro handle
    ;;  We don't reuse Racket's string reader
    ;;  because we don't want to depend on non-public API,
    ;;  and because Emacs has different backslash escape sequences.
    #\" 'terminating-macro handle
    ;;  Character literals.
    #\? 'non-terminating-macro handle
))

;;  --------------------    read

;;  The caller should set the "port-count-lines-enabled" parameter to #t,
;;  and set the "current-source" parameter to the path of the input file.
;;
;;  Quagmire: Why does using "read" cause source to be #f?
;;  We have to resort to "read-syntax" and "syntax->datum".
(define (my-read (in (current-input-port)))
    (define syn
        (parameterize ( (current-readtable my-readtable) )
            (read-syntax (current-source) in)))
    (if (eof-object? syn) eof (syntax->datum syn)))

(define (read-until-eof (in (current-input-port)))
    (for/list ((item (in-port my-read in))) item))

(define (my-with-input-from-file path action)
    (parameterize ( (port-count-lines-enabled #t)
                    (current-source path) )
        (with-input-from-file (current-source) action)))
