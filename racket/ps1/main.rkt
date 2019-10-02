#lang racket

;;  Resources:
;;  https://svkt.org/~simias/guide.pdf
;;  https://problemkaputt.de/psx-spx.htm
;;  http://hitmen.c02.at/files/docs/psx/psx.pdf

(provide (all-defined-out))
(require "syntax.rkt")

;;  --------------------    Settings.

(define BIOS_FILE_PATH "/home/erik/.pcsx/bios/SCPH1001.BIN")

;;  --------------------    Ports.

(DEFINE PORT EXP_1_BASE     "Expansion 1 Base Address"  AT #x1F801000 SIZE 4 ON WRITE EXPECT VALUE #x1F000000)
(DEFINE PORT EXP_2_BASE     "Expansion 2 Base Address"  AT #x1F801004 SIZE 4 ON WRITE EXPECT VALUE #x1F802000)
(DEFINE PORT EXP_1_DELAY    "Expansion 1 Delay/Size"    AT #x1F801008 SIZE 4 ON WRITE EXPECT VALUE #x0013243F)
(DEFINE PORT EXP_3_DELAY    "Expansion 3 Delay/Size"    AT #x1F80100C SIZE 4 ON WRITE EXPECT VALUE #x00003022)
(DEFINE PORT BIOS_ROM_DELAY "BIOS ROM Delay/Size"       AT #x1F801010 SIZE 4 ON WRITE EXPECT VALUE #x0013243F)
(DEFINE PORT SPU_DELAY      "SPU Delay/Size"            AT #x1F801014 SIZE 4 ON WRITE EXPECT VALUE #x200931E1)
(DEFINE PORT CDROM_DELAY    "CDROM Delay/Size"          AT #x1F801018 SIZE 4 ON WRITE EXPECT VALUE #x00020843)
(DEFINE PORT EXP_2_DELAY    "Expansion 2 Delay/Size"    AT #x1F80101C SIZE 4 ON WRITE EXPECT VALUE #x00070777)
(DEFINE PORT COM_DELAY      "COM Delay/Size?"           AT #x1F801020 SIZE 4 ON WRITE EXPECT VALUE #x00031125)
(DEFINE PORT RAM_SIZE       "RAM Size"                  AT #x1F801060 SIZE 4 ON WRITE EXPECT VALUE #x00000B88)
(DEFINE PORT MAIN_VOL_L     "Main Volume Left"          AT #x1F801D80 SIZE 2)
(DEFINE PORT MAIN_VOL_R     "Main Volume Right"         AT #x1F801D82 SIZE 2)
(DEFINE PORT REV_VOL_L      "Reverb Volume Left"        AT #x1F801D84 SIZE 2)
(DEFINE PORT REV_VOL_R      "Reverb Volume Right"       AT #x1F801D86 SIZE 2)
(DEFINE PORT BOOT_DISP      "Boot Status 7-Seg Display" AT #x1F802041 SIZE 1)
(DEFINE PORT CACHE_CONTROL  "Cache Control"             AT #x1FFE0130 SIZE 4)

;;  --------------------    Memory.

(struct Memory_Region (read write) #:prefab)

(define (bytes_read_unsigned #:from bytes #:at offset #:size size)
    (define signed? #f)
    (define big-endian? #f)
    (integer-bytes->integer bytes signed? big-endian? offset (+ offset size)))

(define (bytes_write_unsigned value #:to bytes #:at offset #:size size)
    (define signed? #f)
    (define big-endian? #f)
    (integer->integer-bytes value size signed? big-endian? bytes offset)
    (void))

(define (make_Region_from_bytes bytes #:base [base 0])
    (define (read size address)
        (bytes_read_unsigned #:from bytes #:at (- address base) #:size size)
    )
    (define (write size address value)
        (bytes_write_unsigned value #:to bytes #:at (- address base) #:size size)
    )
    (Memory_Region read write))

(define KILOBYTE 1024)
(define MEGABYTE 1048576)

;;  0x00000000-0x001FFFFF   kuseg of RAM
;;  0x80000000-0x801FFFFF   kseg0, alias for cached access
;;  0xA0000000-0xA01FFFFF   kseg1, alias for uncached access

(define RAM_size (* 2 MEGABYTE))
(define RAM_bytes (make-bytes RAM_size))

;;  XXX: This should depend on the value of port 0x1F801060 (RAM_SIZE).

(define RAM_Region_0 (make_Region_from_bytes RAM_bytes #:base #x00000000))
(define RAM_Region_1 (make_Region_from_bytes RAM_bytes #:base #x00200000))
(define RAM_Region_2 (make_Region_from_bytes RAM_bytes #:base #x00400000))
(define RAM_Region_3 (make_Region_from_bytes RAM_bytes #:base #x00600000))

;;  0x1F800000-0x1F8003FF

(define SCRATCH_PAD_Region (LOCAL
    (define bytes (make-bytes (* 1 KILOBYTE)))
    (make_Region_from_bytes bytes #:base #x1F800000)))

;;  0x1FC00000-0x1FC7FFFF   kuseg of BIOS ROM
;;  0x9FC00000-0x9FC7FFFF   kseg0, alias for cached access
;;  0xBFC00000-0xBFC7FFFF   kseg1, alias for uncached access

(define BIOS_ROM_Region (LOCAL
    (define BIOS_ROM_SIZE (* 512 KILOBYTE))
    (define bytes (file->bytes BIOS_FILE_PATH #:mode 'binary))
    (unless (= (bytes-length bytes) BIOS_ROM_SIZE)
        (error "BIOS file size must be 512 KB"))
    (make_Region_from_bytes bytes #:base #x1FC00000)))

;;  No memory management unit (MMU).

;;  Translate virtual address to physical address.

(define (translate_address address) (bitwise-and address #x1FFFFFFF))

;;  size is 1, 2, or 4.

(define (check_align func address #:size size)
    (unless (zero? [modulo address size])
        (error func "unaligned ~a-byte access from/to address ~a" size [hex4 address])))

(define Ports_Region (LOCAL
    (define (read size address)
        (read_port #:address address #:size size))
    (define (write size address value)
        (write_port #:address address #:size size #:value value))
    (Memory_Region read write)))

(define Zero_Region (LOCAL
    (define (read size address) 0)
    (define (write size address value) (void))
    (Memory_Region read write)))

(define (select_region phys)
    (cond
        ([<= #x00000000 phys #x001FFFFF]        RAM_Region_0)
        ([<= #x00200000 phys #x003FFFFF]        RAM_Region_1)
        ([<= #x00400000 phys #x005FFFFF]        RAM_Region_2)
        ([<= #x00600000 phys #x007FFFFF]        RAM_Region_3)
        ([<= #x1F000000 phys #x1F7FFFFF]        Zero_Region)
        ([<= #x1F800000 phys #x1F8003FF]        SCRATCH_PAD_Region)
        ([<= #x1F801000 phys #x1F803FFF]        Ports_Region)
        ([<= #x1FC00000 phys #x1FC7FFFF]        BIOS_ROM_Region)
        ([<= #x1FFE0000 phys #x1FFE01FF]        Ports_Region)
        (else                                   Invalid_Region)))

(define Invalid_Region (LOCAL
    (define (read size address)
        (error 'mem_read "~a-byte read from invalid physical address ~a" size (hex4 address)))
    (define (write size address value)
        (error 'mem_write "~a-byte write ~a to invalid physical address ~a" size (hex4 value) (hex4 address)))
    (Memory_Region read write)))

(define (mem_read #:from address #:size size)
    (check_align 'mem_read address #:size size)
    (define phys (translate_address address))
    (define region (select_region phys))
    ([Memory_Region-read region] size phys))

(define (mem_write value #:to address #:size size)
    (check_align 'mem_write address #:size size)
    (define phys (translate_address address))
    (define region (select_region phys))
    ([Memory_Region-write region] size phys value))

;;  --------------------    Instruction decoding.

(define (decode ins)
    (define op (bitwise-bit-field ins 26 32)) ;; I,J,R
    (define rs (bitwise-bit-field ins 21 26)) ;; I,R
    (define rt (bitwise-bit-field ins 16 21)) ;; I,R
    (define immediate (bitwise-bit-field ins 0 16)) ;; I
    (define target (bitwise-bit-field ins 0 26)) ;; J
    (define rd (bitwise-bit-field ins 11 16)) ;; R
    (define shamt (bitwise-bit-field ins 6 11)) ;; R
    (define funct (bitwise-bit-field ins 0 6)) ;; R
    (define NOT_IMPLEMENTED `(not_implemented ,op ,rs ,rt ,rd ,shamt ,funct))
    (define (sign_extend x)
        (if (bitwise-bit-set? x 15)
            (- (- #x10000 x))
            x
        ))
    (define offset
        (arithmetic-shift
            (if (bitwise-bit-set? immediate 15)
                (sign_extend immediate)
                immediate)
            2
        ))
    (define (pc_offset) `(add_u32 pc ,offset))
    (define (pc_26) `[or (and pc #xF0000000) ,(arithmetic-shift target 2)])
    (define (ls_addr)
        `(add_u32 (r ,rs) ,(sign_extend immediate)))
    (case op
        ([#b000000]
            (case funct
                ([#b000000] `(set (r ,rd) [sll (r ,rt) ,shamt]))
                ([#b000010] `(set (r ,rd) [srl (r ,rt) ,shamt]))
                ([#b000011] `(set (r ,rd) [sra (r ,rt) ,shamt]))
                ([#b001000] `[begin_jump_to (r ,rs)]) ;; JR
                ([#b001001] `[begin (set (r ,rd) (add_u32 pc 8)) (begin_jump_to (r ,rs))]) ;; JALR
                ([#b100000] `(set (r ,rd) [add_s32 (r ,rs) (r ,rt)])) ;; ADD
                ([#b100001] `(set (r ,rd) [add_u32 (r ,rs) (r ,rt)])) ;; ADDU
                ([#b100010] `(set (r ,rd) [trapping_sub (r ,rs) (r ,rt)])) ;; SUB
                ([#b100011] `(set (r ,rd) [- (r ,rs) (r ,rt)])) ;; SUBU
                ([#b100100] `(set (r ,rd) [and (r ,rs) (r ,rt)]))
                ([#b100101] `(set (r ,rd) [or (r ,rs) (r ,rt)]))
                ([#b100110] `(set (r ,rd) [xor (r ,rs) (r ,rt)]))
                ([#b100111] `(set (r ,rd) [nor (r ,rs) (r ,rt)]))
                ([#b101010] `(set (r ,rd) [lt_s32 (r ,rs) (r ,rt)])) ;; SLT
                ([#b101011] `(set (r ,rd) [lt_u32 (r ,rs) (r ,rt)])) ;; SLTU
                (else NOT_IMPLEMENTED)
            )
        )
        ([#b000010] `[begin_jump_to ,(pc_26)]) ;; J
        ([#b000011] `[begin ;; JAL
                (set (r 31) (add_u32 pc 8))
                (begin_jump_to ,(pc_26))])
        ([#b000100] `[if (= (r ,rs) (r ,rt)) (begin_jump_to ,(pc_offset))]) ;; BEQ
        ([#b000101] `[if (!= (r ,rs) (r ,rt)) (begin_jump_to ,(pc_offset))]) ;; BNE
        ([#b000110] `[if (= (r ,rs) 0) (begin_jump_to ,(pc_offset))]) ;; BLEZ
        ([#b000111] `[if (gt_s32 (r ,rs) 0) (begin_jump_to ,(pc_offset))]) ;; BGTZ
        ([#b001000] `(set (r ,rt) [add_s32 (r ,rs) ,(sign_extend immediate)])) ;; ADDI
        ([#b001001] `(set (r ,rt) [add_u32 (r ,rs) ,immediate])) ;; ADDIU
        ([#b001100] `(set (r ,rt) [and (r ,rs) ,immediate]))
        ([#b001101] `(set (r ,rt) [or (r ,rs) ,immediate]))
        ([#b001110] `(set (r ,rt) [xor (r ,rs) ,immediate]))
        ([#b001111] `(set (r ,rt) ,(arithmetic-shift immediate 16))) ;; LUI
        ([#b010000] ;; COP0
            (case rs
                ([#b00000] `[set (r ,rt) (c 0 d ,rd)]) ;; MFC0
                ([#b00010] `[set (r ,rt) (c 0 c ,rd)]) ;; CFC0
                ([#b00100] `[set (c 0 d ,rd) (r ,rt)]) ;; MTC0
                ([#b00110] `[set (c 0 c ,rd) (r ,rt)]) ;; CTC0
                (else NOT_IMPLEMENTED)
            ))
       ;([#b010001] ) ;; COP1
       ;([#b010010] ) ;; COP2
       ;([#b010011] ) ;; COP3
        ([#b100000] `(load [r ,rt] [ms8 ,(ls_addr)])) ;; LB
        ([#b100001] `(load [r ,rt] [ms16 ,(ls_addr)])) ;; LH
       ;([#b100010] ) ;; LWL
        ([#b100011] `(load [r ,rt] [mu32 ,(ls_addr)])) ;; LW
        ([#b100100] `(load [r ,rt] [mu8 ,(ls_addr)])) ;; LBU
        ([#b100101] `(load [r ,rt] [mu16 ,(ls_addr)])) ;; LHU
       ;([#b100110] ) ;; LWR
        ([#b101000] `(set [mu8 ,(ls_addr)] [r ,rt])) ;; SB
        ([#b101001] `(set [mu16 ,(ls_addr)] [r ,rt])) ;; SH
       ;([#b101010] ) ;; SWL
        ([#b101011] `(set [mu32 ,(ls_addr)] [r ,rt])) ;; SW
       ;([#b101110] ) ;; SWR
        (else NOT_IMPLEMENTED)
    )
)

;;  --------------------    Register file.

(define REGS (make-vector 32 0))
(define (set_reg! index value)
    (when (and [= index 0] [!= value 0])
        (error 'set_reg! "writing non-zero value ~a to r0" [hex4 value]))
    (vector-set! REGS index value))
(define (get_reg index)         (vector-ref REGS index))
(define (dump_registers)
    (define regs_per_row 8)
    (for ([index (in-naturals)] [value (in-vector REGS)])
        (define-values (row col) (quotient/remainder index regs_per_row))
        (when [and (> row 0) (= col 0)]
            (newline))
        (printf "~a ~a  "
            (~a "r" index #:width 3)
            (~r value #:base 16 #:min-width 8 #:pad-string "0")))
    (newline))

;;  --------------------    Execution.

(define (add_u32 x y) (bitwise-and (+ x y) #xFFFFFFFF))
(define (u32->s x)
    (if (bitwise-bit-set? x 31)
        (- x #x100000000)
        x
    ))
(define (s->u32 x) (bitwise-and x #xFFFFFFFF))
(define (s8->u32 x) (bitwise-and (- x #x100) #xFFFFFFFF))
(define (s16->u32 x) (bitwise-and (- x #x10000) #xFFFFFFFF))
(define (s32->u32 x) (bitwise-and (- x #x100000000) #xFFFFFFFF))
(define (unbool x) (if x 1 0))
(define (lt_u32 x y) [unbool (< x y)])
(define (gt_u32 x y) [unbool (> x y)])
(define (lt_s32 x y) [unbool (< (u32->s x) (u32->s y))])
(define (gt_s32 x y) [unbool (> (u32->s x) (u32->s y))])
(define (add_s32 x y)
    (define z (+ (u32->s x) (u32->s y)))
    (define minx [- (arithmetic-shift 1 32)])
    (define maxx (arithmetic-shift 1 31))
    (unless [< minx z maxx] [error 'add_s32 "two's complement overflow: ~a + ~a" (hex4 x) (hex4 y)])
    (s->u32 z))
;;  get_operand returns a u32.
(define (get_operand op)
    (match op
        ([? number? immediate]      (s->u32 immediate))
        (`[r ,index]                (get_reg index))
        (`[add_u32 ,a ,b]           (add_u32 [get_operand a] [get_operand b]))
        (`[add_s32 ,a ,b]           (add_s32 [get_operand a] [get_operand b]))
        (`[sll ,a ,b]               (arithmetic-shift [get_operand a] [get_operand b]))
        (`[and ,a ,b]               (bitwise-and [get_operand a] [get_operand b]))
        (`[or ,a ,b]                (bitwise-ior [get_operand a] [get_operand b]))
        (`[= ,a ,b]                 (unbool (= [get_operand a] [get_operand b])))
        (`[!= ,a ,b]                (unbool (!= [get_operand a] [get_operand b])))
        (`[lt_u32 ,a ,b]            (lt_u32 [get_operand a] [get_operand b]))
        (`[gt_u32 ,a ,b]            (gt_u32 [get_operand a] [get_operand b]))
        (`[lt_s32 ,a ,b]            (lt_s32 [get_operand a] [get_operand b]))
        (`[gt_s32 ,a ,b]            (gt_s32 [get_operand a] [get_operand b]))
        (`[ms8 ,addr]               (s8->u32 (mem_read #:from [get_operand addr] #:size 1)))
        (`[ms16 ,addr]              (s16->u32 (mem_read #:from [get_operand addr] #:size 2)))
        (`[ms32 ,addr]              (s32->u32 (mem_read #:from [get_operand addr] #:size 4)))
        (`[mu8 ,addr]               (mem_read #:from [get_operand addr] #:size 1))
        (`[mu16 ,addr]              (mem_read #:from [get_operand addr] #:size 2))
        (`[mu32 ,addr]              (mem_read #:from [get_operand addr] #:size 4))
        (`pc                        PC)
    ))

(define C0C (make-vector 16 0)) ;; XXX how many?
(define C0D (make-vector 16 0)) ;; XXX how many?

(define (set_operand! op val)
    (match op
        (`[r ,dst]      (set_reg! dst val))
        (`[mu8 ,addr]   (mem_write (bitwise-and val #xFF) #:to (get_operand addr) #:size 1))
        (`[mu16 ,addr]  (mem_write (bitwise-and val #xFFFF) #:to (get_operand addr) #:size 2))
        (`[mu32 ,addr]  (mem_write val #:to (get_operand addr) #:size 4))
        (`[c 0 c ,n]    (vector-set! C0C n val))
        (`[c 0 d ,n]    (vector-set! C0D n val))
    ))

(define PC_after_reset #xBFC00000)

(DEFINE STATE PC PC_after_reset) ;; program counter
(DEFINE STATE in_branch_delay_slot #f)
(DEFINE STATE jump_target 0)
(DEFINE STATE loading #f)
(DEFINE STATE load_dst '())
(DEFINE STATE load_src '())

(define (execute dec)
    (define in_bds #f)
    (define _loading #f)
    (define ld_dst #f)
    (define ld_src #f)
    (define (~R width value) (~r value #:base 2 #:min-width width #:pad-string "0"))
    (when loading (set_operand! load_dst load_src))
    (define (execute0 dec)
        (match dec
            (`[set ,dst ,src]
                (set_operand! dst (get_operand src)))
            (`[load ,dst ,src]
                (set! _loading #t)
                (set! ld_dst dst)
                (set! ld_src (get_operand src)))
            (`[begin_jump_to ,target]
                (when in_branch_delay_slot
                    (error 'execute "branch in branch delay slot ~a" [hex4 PC]))
                (set! in_bds #t)
                (set! jump_target (get_operand target)))
            (`[if ,cond ,then]
                (when (!= (get_operand cond) 0)
                    (execute0 then)
                ))
            ([cons 'begin exprs]
                (for ([expr (in-list exprs)]) (execute0 expr)))
            (`[not_implemented ,op ,rs ,rt ,rd ,shamt ,funct]
                (error 'decode "~a: ~a ~a ~a ~a ~a ~a"
                    dec
                    (~R 6 op)
                    (~R 5 rs)
                    (~R 5 rt)
                    (~R 5 rd)
                    (~R 5 shamt)
                    (~R 6 funct)
                ))))
    (execute0 dec)
    (set! in_branch_delay_slot in_bds)
    (set! loading _loading)
    (set! load_dst ld_dst)
    (set! load_src ld_src)
)

;;  XXX

(define (display_instruction ins dec)
    ;;  TODO: friendly coprocessor register names
    (define (pretty dec)
        (match dec
            [(? number? n) (string-append "#" (~r n #:base 16))]
            [`(r ,index) (format "r~a" index)]
            [`(c ,cop ,type ,index) (format "c~a~a~a" cop type index)]
            [`(add_u32 ,a ,b) `(add_u32 ,(pretty a) ,(pretty b))]
            [`(mu32 ,addr) `(mu32 ,(pretty addr))]
            [`(set ,dst ,src) `(set ,(pretty dst) ,(pretty src))]
            [`(load ,dst ,src) `(load ,(pretty dst) ,(pretty src))]
            [`(if ,cond ,action) `(if ,cond ,(pretty action))]
            [`(begin_jump_to ,target) `(begin_jump_to ,(pretty target))]
            [_ dec]))
    (define (evaluate_partially dec)
        (define (f dec)
            (match dec
                [`(set (r ,dst) ,src) `(set (r ,dst) ,(get_operand src))]
                [`(load ,dst (mu32 ,src)) `(load ,dst (mu32 ,(get_operand src)))]
                [`(set (mu32 ,dst) ,src) `(set (mu32 ,(get_operand dst)) ,(get_operand src))]
                [`(if ,cond ,action) `(if ,(get_operand cond) ,(f action))]
                [`(begin_jump_to ,target) `(begin_jump_to ,(get_operand target))]
                [_ dec]))
        (f dec))
    ;;  Partial-evaluation improves the readability of the execution trace.
    (define WANT_PARTIAL_EVALUATION #t)
    (define ins_to_print
        (if WANT_PARTIAL_EVALUATION
            (evaluate_partially dec)
            dec))
    (printf "~a ~a ~a" [hex4 PC] [hex4 ins] (pretty ins_to_print))
    (when loading
        (printf " ;; but first set ~a to ~a" [pretty load_dst] [pretty load_src]))
    (when in_branch_delay_slot
        (printf " ;; and then jump to ~a" [pretty jump_target]))
    (newline))

(DEFINE STATE BREAKPOINTS (make-hash))
(define (add_breakpoint_at! address) (hash-set! BREAKPOINTS address #t))
(define (clear_breakpoints!) (hash-clear! BREAKPOINTS))
(define (remove_breakpoint_at! address) (hash-remove! BREAKPOINTS address))
(define (has_breakpoint_at? address) (hash-ref BREAKPOINTS address #f))

;;  TODO: Auto-save-state to default file on recoverable exception (breakpoint, decode not_implemented).

(define (step)
    ;;  ----------  Do not change any state, so that the code can be restarted on error.
    ;;  PC contains the address of currently executing instruction.
    (when (has_breakpoint_at? PC)
        ;;  TODO: Custom exception.
        (error 'step "breakpoint ~a" [hex4 PC]))
    (define ins (mem_read #:from PC #:size 4))
    (define dec (decode ins))
    (define next_PC (if in_branch_delay_slot jump_target (+ PC 4)))
    ;;  ----------  State-change is allowed after this point.
    ;;  State change is allowed beyond this point.
    (when TRACE_ENABLED
        ;(dump_registers)
        (display_instruction ins dec))
    (execute dec)
    (set! PC next_PC)
)

(define cpu_thread (LOCAL
    ;;  Ideally one batch takes a few hundred milliseconds of real time.
    (define batch_size 16384)
    (define running #f)
    (define (handle message)
        (match message
            ['resume (set! running #t)]
            ['pause (set! running #f)]
            [_ (void)]))
    (define (main)
        (if running
            (begin
                (run_some batch_size)
                (handle (thread-try-receive))
            )
            (handle (thread-receive))
        )
        (main))
    (thread main)))

;;  --------------------    Interaction commands.

(define (run_some num_ins)
    (for ([i (in-range num_ins)])
        (step)
    ))
;;  How do we implement this?
;;  Add one step before decode.
(define (disassemble #:from [begin PC] #:count [count 8])
    (error 'disassemble "not implemented")
)
(define (resume) (thread-send cpu_thread 'resume))
(define (pause) (thread-send cpu_thread 'pause))
(define (trace) (set_trace_enabled! #t))
(define (notrace) (set_trace_enabled! #f))

;;  --------------------    Load-save state.

(GENERATE READ WRITE STATE FUNCTION read_state write_state)

;; DEBUG

(define break_at! add_breakpoint_at!)
(define (run) (step) (run))

(define (main)
    (trace)
    (break_at! #xBFC003B4)
    ;;  TODO: load-save state
    (run)
)
