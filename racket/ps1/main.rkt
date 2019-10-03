#lang racket

(require "user.rkt")

(provide (all-defined-out))
(provide (all-from-out "user.rkt"))

;;  Do we need a CPU thread?
;;  we can run it on the main thread.

(define cpu_thread (LOCAL
    ;;  Ideally one batch takes a few hundred milliseconds of real time.
    (define batch_size 16384)
    (define running #f)
    (define (handle message)
        (match message
            ['resume (set! running #t)]
            ['pause (set! running #f)]
            [`(step ,count)
                ;;  Because break is disabled on the CPU thread,
                ;;  if this takes too long, the program will seem unresponsive.
                (if (<= count batch_size)
                    (begin
                        (set! running #t)
                        (run_at_most count)
                        (set! running #f)
                    )
                    (printf "ERROR: Count ~a exceeds limit ~a" count batch_size)
                )]
            [_ (void)]))
    (define (main)
        (break-enabled #f)
        (if running
            (begin
                (run_at_most batch_size)
                (handle (thread-try-receive))
            )
            (handle (thread-receive))
        )
        (main))
    (thread main)))

;;  --------------------    Interaction commands.

(define (disasm #:from [begin PC] #:count [count 8])
    (for ((n (in-range count)))
        (define address (+ PC (* n 4)))
        (define ins (mem_read #:from address #:size 4))
        (printf "~a ~a ~a~n" [hex4 PC] [hex4 ins] [disassemble_for_display ins])
    ))
(define (resume) (thread-send cpu_thread 'resume))
(define (ct_step count) (thread-send cpu_thread `(step ,count)))
(define (pause) (thread-send cpu_thread 'pause))

(init_session!)

;;  Problem: We don't limit the history file size.

(require readline/readline)

(define HISTORY_FILE_PATH "_history")

(define-values (history_input history_output)
    (open-input-output-file HISTORY_FILE_PATH #:mode 'text #:exists 'append))

(define (read_history)
    (for ([line (in-lines history_input)])
        (add-history line)))

(define (append_history line)
    (add-history line)
    (displayln line history_output))

;;  TODO: Auto-save-state to default file on recoverable exception (breakpoint, decode not_implemented).

(define (main)
    (read_history)
    (define (on_break e)
        (printf "Break~n")
        (dump_quick)
    )
    (define (on_breakpoint e)
        (define address (exn_breakpoint_address e))
        (printf "Breakpoint ~a hit~n" [hex4 address])
        (dump_quick)
        (remove_breakpoint_at! address)
    )
    (define (read_argument name port)
        (define x (read port))
        (when (eof-object? x)
            (error 'read_argument "while reading ~a: end of input" name))
        x)
    (struct Command (read_args function))
    (define-syntax-rule (COMMAND [Key Param ...] Body ...)
        (let (
                [Read_Args (lambda (port)
                    (list (read_argument 'Param port) ...)
                )]
                [Func (lambda (Param ...) Body ...)]
            )
            (cons 'Key (Command Read_Args Func))))
    (define running #t)
    (define HELP #<<END_HELP
------------    Interaction

<empty line>    Repeat last command
h               Show this help
q               Quit
qs              Quit after saving state to default state file
sl              Load state from default state file (See "default_state_snapshot_path" variable)
ss              Save state to default state file
sdel            Delete default state file

------------    Basic Movement

d               Dump registers and next instruction
c               Continue/run/resume/start until break (Ctrl+C)
cl Num          Step (execute) with limit of Num instructions
s               Step one instruction

------------    Dump, Display, Disassemble

dl0             Set disassembly level 0 (MIPS instructions)
dl1             Set disassembly level 1 (Lisp-like fragments)
dl2             Set disassembly level 2 (level 1 with partial evaluation)
te              Enable tracing
td              Disable tracing

------------    Advanced Movement, Breakpoint, Running

bs Addr         Set breakpoint at address
bl              List breakpoint addresses
cda             Run to different/unvisited address (for getting out of loop)
cdac            Clear list of visited addresses
cdal            List visited addresses

------------    Not Implemented But Perhaps Nice To Have

cnb             Run to next branch instruction
cnc             Run to next coprocessor register access (read or write)
cnp             Run to next IO port access (read or write)
bbd             Decompile nearest basic block
END_HELP
)
    (define visited_addresses (make-hash))
    (define (run_to_different_address)
        (if (hash-ref visited_addresses PC #f)
            (begin
                (run_at_most 1)
                (run_to_different_address))
            (hash-set! visited_addresses PC #t)))
    (define commands (make-hash (list
        (COMMAND (q) (set! running #f))
        (COMMAND (qs) (save_state) (set! running #f))
        (COMMAND (sdel) (delete_state))
        (COMMAND (h) (displayln HELP))
        (COMMAND (d) (dump_quick))
        (COMMAND (dl0) (set_disassembly_level! 0))
        (COMMAND (dl1) (set_disassembly_level! 1))
        (COMMAND (dl2) (set_disassembly_level! 2))
        (COMMAND (bs address) (set_breakpoint_at! address))
        (COMMAND (bl)
            (for-each [lambda (addr) (printf "#x~a~n" (hex4 addr))]
                (get_breakpoint_addresses)))
        (COMMAND (c) (run_unlimited))
        (COMMAND (cl count) (run_at_most count) (dump_quick))
        (COMMAND (cda) (run_to_different_address) (dump_quick))
        (COMMAND (cdac) (hash-clear! visited_addresses))
        (COMMAND (cdal) (for-each (lambda (k) (printf "~a~n" [hex4 k])) (hash-keys visited_addresses)))
        (COMMAND (s) (run_at_most 1) (dump_quick))
        (COMMAND (sl) (load_state))
        (COMMAND (ss) (save_state))
        (COMMAND (te) (trace))
        (COMMAND (td) (notrace))
    )))
    (define (show_exception e)
        ((error-display-handler)
            (if (exn? e) (exn-message e) "error in main.top:")
            e))
    (define last_command "")
    (define (top)
        (define (top_0)
            (define PROMPT "ps1> ")
            (define line (readline PROMPT))
            ;;  Ctrl+D quits.
            (when (eof-object? line) (set! running #f))
            (when running
                (if (equal? line "")
                    (set! line last_command)
                    (begin
                        (set! last_command line)
                        (append_history line)))
                (define port (open-input-string line))
                (define key (read port))
                (define cmd (hash-ref commands key #f))
                (if cmd
                    (apply (Command-function cmd) ((Command-read_args cmd) port))
                    (printf "Unknown command: ~a~n" key))
                ;;  "running" may have been changed by the "q" command.
                (when running (top_0))
            ))
        ;;  Prevent accidental quitting on Ctrl+C.
        (with-handlers [
                (exn:break? on_break)
                (exn_breakpoint? on_breakpoint)
                (exn:fail? show_exception)
            ]
            (top_0))
        (when running (top))
    )
    (top)
)
