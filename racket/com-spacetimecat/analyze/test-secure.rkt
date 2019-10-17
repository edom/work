#lang s-exp "lang.rkt"

;;  Usage:
;;
;;      -   Set up your security restrictions.
;;      -   Load this module with dynamic-require.
;;      -   The file BOOM should not get created.
;;      -   The procedure should-not-run should not run.
;;
;;  It is easy to know if the security is wrong.
;;  It is hard to know if the security is right.
;;
;;  We generally can't do much against security problems in the lower layers,
;;  such as in hardware security problems such as Meltdown/Spectre,
;;  or things as simple as physical access to the underlying hardware,
;;  or threats to the humans who use the machine.

(define (should-not-run port)
    (display "
If the file gets created and this message gets printed,
then there is an obvious security problem.
However, if the file doesn't get created,
there may still be other unknown security problems.
"))

(call-with-output-file "BOOM" should-not-run)
