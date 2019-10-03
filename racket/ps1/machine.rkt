#lang racket

;;  Resources:
;;  https://svkt.org/~simias/guide.pdf
;;  https://problemkaputt.de/psx-spx.htm
;;  http://hitmen.c02.at/files/docs/psx/psx.pdf

(require "syntax.rkt")

(provide
    mem_read
    PC
    run_at_most
    run_unlimited
    disassemble_for_display
    set_disassembly_level!
    dump_quick

    BIOS_ROM_MIN_ADDRESS
    BIOS_ROM_MAX_ADDRESS_1
    decode_instruction_at

    set_breakpoint_at!
    remove_breakpoint_at!
    get_breakpoint_addresses

    exn_decode?
    exn_decode_cause
    make_exn_decode

    exn_breakpoint?
    exn_breakpoint_address
    make_exn_breakpoint
)
(provide (all-from-out "syntax.rkt"))

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

(struct Memory_Region (read write))

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

(define BIOS_ROM_SIZE (* 512 KILOBYTE))

(DEFINE STATE MEMORIES
    (vector
        (make-bytes [* 2 MEGABYTE]) ;; RAM
        (make-bytes [* 1 KILOBYTE]) ;; Scratch Pad
        (make-bytes BIOS_ROM_SIZE) ;; BIOS ROM
    )
    AFTER RESTORE DO (initialize_regions!)
)

(define (RAM_bytes) (vector-ref MEMORIES 0))
(define (SCRATCH_PAD_bytes) (vector-ref MEMORIES 1))
(define (BIOS_ROM_bytes) (vector-ref MEMORIES 2))

(define RAM_Region_0 #f)
(define RAM_Region_1 #f)
(define RAM_Region_2 #f)
(define RAM_Region_3 #f)
(define SCRATCH_PAD_Region #f)
(define BIOS_ROM_Region #f)
(define Ports_Region (LOCAL
    (define (read size address) (read_port #:address address #:size size))
    (define (write size address value) (write_port #:address address #:size size #:value value))
    (Memory_Region read write)))
(define Zero_Region (LOCAL
    (define (read size address) 0)
    (define (write size address value) (void))
    (Memory_Region read write)))
(define Invalid_Region (LOCAL
    (define (read size address) (error 'mem_read "~a-byte read from invalid physical address ~a" size (hex4 address)))
    (define (write size address value) (error 'mem_write "~a-byte write ~a to invalid physical address ~a" size (hex4 value) (hex4 address)))
    (Memory_Region read write)))

(define (load_bios_rom!)
    (define bytes (file->bytes BIOS_FILE_PATH #:mode 'binary))
    (unless (= (bytes-length bytes) BIOS_ROM_SIZE)
        (error "BIOS file size must be 512 KB"))
    (bytes-copy! (BIOS_ROM_bytes) 0 bytes))

(load_bios_rom!)

(define BIOS_ROM_MIN_ADDRESS   #x1FC00000)
(define BIOS_ROM_MAX_ADDRESS_1 #x1FC80000)

;;  This mutation is for load-save state.

(define (initialize_regions!)
    ;;  XXX: This should depend on the value of port 0x1F801060 (RAM_SIZE).
    (set! RAM_Region_0 (make_Region_from_bytes (RAM_bytes) #:base #x00000000))
    (set! RAM_Region_1 (make_Region_from_bytes (RAM_bytes) #:base #x00200000))
    (set! RAM_Region_2 (make_Region_from_bytes (RAM_bytes) #:base #x00400000))
    (set! RAM_Region_3 (make_Region_from_bytes (RAM_bytes) #:base #x00600000))
    ;;  0x1F800000-0x1F8003FF
    (set! SCRATCH_PAD_Region (make_Region_from_bytes (SCRATCH_PAD_bytes) #:base #x1F800000))
    ;;  0x1FC00000-0x1FC7FFFF   kuseg of BIOS ROM
    ;;  0x9FC00000-0x9FC7FFFF   kseg0, alias for cached access
    ;;  0xBFC00000-0xBFC7FFFF   kseg1, alias for uncached access
    (set! BIOS_ROM_Region (make_Region_from_bytes (BIOS_ROM_bytes) #:base #x1FC00000))
)

(initialize_regions!)

;;  No memory management unit (MMU).

;;  Translate virtual address to physical address.

(define (translate_address address) (bitwise-and address #x1FFFFFFF))

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

;;  size is 1, 2, or 4.

(define (check_align func address #:size size)
    (unless (zero? [modulo address size])
        (error func "unaligned ~a-byte access from/to address ~a" size [hex4 address])))

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

(require "instruction.rkt")

(define (prettify_disassembly thing)
    (define (pretty thing)
        (match thing
            (`[r ,n] (~a 'r n))
            (`[c ,c ,type ,index] (~a 'c c type index))
            (`[+ ,a ,b] (~a "(" (pretty a) '+ (pretty b) ")"))
            (`[,ins ,a ,b ,c] (list ins (pretty a) (pretty b) (pretty c)))
            (`[,ins ,a ,b] (list ins (pretty a) (pretty b)))
            ([? number? n]
                (~a "#"
                    (cond
                        ([<= (abs n) #xFF] (hex1 n))
                        ([<= (abs n) #xFFFF] (hex2 n))
                        ([<= (abs n) #xFFFFFFFF] (hex4 n))
                        (else n))))
            (_ thing)))
    (pretty thing))

(define (disassemble_for_display ins) (prettify_disassembly (disassemble ins)))

;;  --------------------    Register file.

(DEFINE STATE REGS (make-vector 32 0))
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

(define (dump_quick)
    (dump_registers)
    (dump_next_instruction))

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
        (`[+ ,a ,b]                 (add_u32 [get_operand a] [get_operand b]))
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

(DEFINE STATE C0D (make-vector 16 0))

(define (set_operand! op val)
    (match op
        (`[r ,dst]      (set_reg! dst val))
        (`[mu8 ,addr]   (mem_write (bitwise-and val #xFF) #:to (get_operand addr) #:size 1))
        (`[mu16 ,addr]  (mem_write (bitwise-and val #xFFFF) #:to (get_operand addr) #:size 2))
        (`[mu32 ,addr]  (mem_write val #:to (get_operand addr) #:size 4))
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

;;  See the usage in "display_before_execution".
;;
;;  Levels:
;;      0 = Lisp lists resembling MIPS disassembly
;;      1 = Lispy fragment equivalents
;;      2 = Level 1 with partial evaluation

(define DISASSEMBLY_LEVEL 0)
(define (set_disassembly_level! x) (set! DISASSEMBLY_LEVEL x))

(define (display_before_execution ins das dec)
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
    (define ins_to_print
        (case DISASSEMBLY_LEVEL
            [(0) (prettify_disassembly das)]
            [(1) (pretty dec)]
            [(2) (pretty (evaluate_partially dec))]
            [else (error 'display_before_execution "invalid level: ~a" DISASSEMBLY_LEVEL)]))
    (printf "~a ~a ~a" [hex4 PC] [hex4 ins] ins_to_print)
    (when loading
        (printf " ;; but first set ~a to ~a" [pretty load_dst] [pretty load_src]))
    (when in_branch_delay_slot
        (printf " ;; and then jump to ~a" [pretty jump_target]))
    (newline))

(struct Exn_Decode (cause) #:prefab)
(define (exn_decode? x) (Exn_Decode? x))
(define (exn_decode_cause x) (Exn_Decode-cause x))
(define (make_exn_decode cause) (Exn_Decode cause))

(struct Exn_Breakpoint (address) #:prefab)
(define (exn_breakpoint? x) (Exn_Breakpoint? x))
(define (exn_breakpoint_address x) (Exn_Breakpoint-address x))
(define (make_exn_breakpoint address) (Exn_Breakpoint address))

(DEFINE STATE ADDRESS_BREAKPOINT (make-hash))
(define (set_breakpoint_at! address) (hash-set! ADDRESS_BREAKPOINT address #t))
(define (clear_breakpoints!) (hash-clear! ADDRESS_BREAKPOINT))
(define (remove_breakpoint_at! address) (hash-remove! ADDRESS_BREAKPOINT address))
(define (has_breakpoint_at? address) (hash-ref ADDRESS_BREAKPOINT address #f))
(define (get_breakpoint_addresses) (hash-keys ADDRESS_BREAKPOINT))

;;  There is some code duplication.

(define (decode_instruction_at address)
    (define ins (mem_read #:from address #:size 4))
    (define das (disassemble ins))
    (decode das)
)

(define (dump_next_instruction)
    (define ins (mem_read #:from PC #:size 4))
    (define das (disassemble ins))
    (define dec (decode das))
    (display_before_execution ins das dec))

;;  "step" is not exported because it assumes that break is disabled.
;;  That is because we want "step" to be atomic, so that the emulator can be paused-and-resumed.

(define (step)
    ;;  ----------  Do not change any state, so that the code can be restarted on error.
    ;;  PC contains the address of currently executing instruction.
    (when (has_breakpoint_at? PC)
        (raise (make_exn_breakpoint PC)))
    (define ins (mem_read #:from PC #:size 4))
    (define das (disassemble ins))
    (define dec (decode das))
    (define next_PC (if in_branch_delay_slot jump_target (+ PC 4)))
    ;;  ----------  State-change is allowed after this point.
    ;;  State change is allowed beyond this point.
    (when TRACE_ENABLED
        ;(dump_registers)
        (display_before_execution ins das dec))
    (execute dec)
    (set! PC next_PC)
)

(define MAX_BATCH_SIZE 16384)

(define (run_at_most num_ins)
    (when (> num_ins MAX_BATCH_SIZE)
        (error 'run_at_most "Count ~a exceeds limit ~a" num_ins MAX_BATCH_SIZE))
    (_run_at_most num_ins))

(define (_run_at_most num_ins)
    ;;  Yielding is important for Racket to respond to user breaks.
    (sleep 0)
    (parameterize-break #f
        (for ([i (in-range num_ins)])
            (step)
        )))

;;  Typically stopped by Ctrl+C.

(define (run_unlimited)
    (_run_at_most MAX_BATCH_SIZE)
    (run_unlimited))
