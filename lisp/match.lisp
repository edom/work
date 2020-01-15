(defpackage+ #:stc/match
  (:documentation "pattern-matching")
  (:use #:common-lisp)
  (:export #:match))

(defmacro match (expr &body clauses)
  "This uses trivia's EMATCH.

  Patterns are documented here:
  https://github.com/guicho271828/trivia/wiki/Various-Patterns
  "
  `(trivia:ematch ,expr ,@clauses))
