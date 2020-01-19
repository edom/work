(in-package #:stc-lisp)

(defun fset (symbol definition)
  "All arguments are evaluated.

SYMBOL is a symbol.

DEFINITION is a symbol that refers to a macro or a function.

Example: (fset 'my-defun 'defun)

Problem: We want the output of DESCRIBE to refer to the location of the FSET form, not of the DEFINITION.
See also: https://github.com/sbcl/sbcl/blob/6def7c51eb4e6e1b3ee12bee1f599529ff046f7d/src/code/describe.lisp#L539

See also:
  - FSET in Emacs Lisp.
  - https://www.gnu.org/software/emacs/manual/html_node/elisp/Function-Indirection.html"
  (let* ((mac (macro-function definition))
         (fun (symbol-function definition)))
    (if mac
      (setf (macro-function symbol) mac)
      (setf (symbol-function symbol) fun)))) ;; or symbol-function?

(defun fdefalias (symbol definition &optional docstring)
"See FSET.

All arguments are evaluated.

See also: DEFALIAS in Emacs Lisp."
  (fset symbol definition)
  (when docstring
    (setf (documentation symbol 'function) docstring)))

(defun list-map (func list)
  (map 'list func list))

(fdefalias 'match 'trivia:ematch
  "This uses trivia's EMATCH.

Patterns are documented here:
https://github.com/guicho271828/trivia/wiki/Various-Patterns")
