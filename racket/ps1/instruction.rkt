#lang racket

(provide
    disassemble
    decode
)

;;  --------------------    Instruction decoding.

(define (disassemble ins)
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
    (define (pc_offset) `(+ pc ,offset))
    (define (pc_26) `[or (and pc #xF0000000) ,(arithmetic-shift target 2)])
    (define Rs `(r ,rs))
    (define Rt `(r ,rt))
    (define Rd `(r ,rd))
    (define (ls_addr)
        `(+ ,Rs ,(sign_extend immediate)))
    (case op
        ([#b000000]
            (case funct
                ([#b000000] `[sll ,Rd ,Rt ,shamt])
                ([#b000010] `[srl ,Rd ,Rt ,shamt])
                ([#b000011] `[sra ,Rd ,Rt ,shamt])
                ([#b001000] `[jr ,Rs])
                ([#b001001] `[jalr ,Rs ,Rd])
                ([#b100000] `[add ,Rd ,Rs ,Rt])
                ([#b100001] `[addu ,Rd ,Rs ,Rt])
                ([#b100010] `[sub ,Rd ,Rs ,Rt])
                ([#b100011] `[subu ,Rd ,Rs ,Rt])
                ([#b100100] `[and ,Rd ,Rs ,Rt])
                ([#b100101] `[or ,Rd ,Rs ,Rt])
                ([#b100110] `[xor ,Rd ,Rs ,Rt])
                ([#b100111] `[nor ,Rd ,Rs ,Rt])
                ([#b101010] `[slt ,Rd ,Rs ,Rt])
                ([#b101011] `[sltu ,Rd ,Rs ,Rt])
                (else NOT_IMPLEMENTED)
            )
        )
        ([#b000010] `[j ,(pc_26)])
        ([#b000011] `[jal ,(pc_26)])
        ([#b000100] `[beq ,Rs ,Rt ,(pc_offset)])
        ([#b000101] `[bne ,Rs ,Rt ,(pc_offset)])
        ([#b000110] `[blez ,Rs ,(pc_offset)])
        ([#b000111] `[bgtz ,Rs ,(pc_offset)])
        ([#b001000] `[addi ,Rt ,Rs ,(sign_extend immediate)])
        ([#b001001] `[addiu ,Rt ,Rs ,immediate])
        ([#b001100] `[andi ,Rt ,Rs ,immediate])
        ([#b001101] `[ori ,Rt ,Rs ,immediate])
        ([#b001110] `[xori ,Rt ,Rs ,immediate])
        ([#b001111] `[lui ,Rt ,immediate])
        ([#b010000] ;; COP0
            (case rs
                ;;  Rt always selects CPU register.
                ;;  Rd always selects COP register.
                ;;  Our convention: The destination is always on the left.
                ([#b00000] `[mfc0 ,Rt (c 0 d ,rd)])
                ([#b00010] `[cfc0 ,Rt (c 0 c ,rd)])
                ([#b00100] `[mtc0 (c 0 d ,rd) ,Rt])
                ([#b00110] `[ctc0 (c 0 c ,rd) ,Rt])
                (else NOT_IMPLEMENTED)
            ))
       ;([#b010001] ) ;; COP1
       ;([#b010010] ) ;; COP2
       ;([#b010011] ) ;; COP3
        ;;  Our convention: The destination is always on the left.
        ([#b100000] `[lb ,Rt ,(ls_addr)])
        ([#b100001] `[lh ,Rt ,(ls_addr)])
       ;([#b100010] ) ;; LWL
        ([#b100011] `[lw ,Rt ,(ls_addr)])
        ([#b100100] `[lbu ,Rt ,(ls_addr)])
        ([#b100101] `[lhu ,Rt ,(ls_addr)])
       ;([#b100110] ) ;; LWR
        ([#b101000] `[sb ,(ls_addr) ,Rt])
        ([#b101001] `[sh ,(ls_addr) ,Rt])
       ;([#b101010] ) ;; SWL
        ([#b101011] `[sw ,(ls_addr) ,Rt]) ;; SW
       ;([#b101110] ) ;; SWR
        (else NOT_IMPLEMENTED)
    )
)

(define (decode das)
    (match das
        (`[sll ,a ,b ,c] `(set ,a [sll ,b ,c]))
        (`[srl ,a ,b ,c] `(set ,a [srl ,b ,c]))
        (`[sra ,a ,b ,c] `(set ,a [sra ,b ,c]))
        (`[jr ,r] `(begin_jump_to ,r))
        (`[jalr ,target ,link] `(begin (set ,link (+ pc 8)) (begin_jump_to ,target)))
        (`[add ,a ,b ,c] `(set ,a [add_s32 ,b ,c]))
        (`[addu ,a ,b ,c] `(set ,a [add_u32 ,b ,c]))
        (`[sub ,a ,b ,c] `(set ,a [sub_s32 ,b ,c]))
        (`[subu ,a ,b ,c] `(set ,a [sub_u32 ,b ,c]))
        (`[and ,a ,b ,c] `[set ,a (and ,b ,c)])
        (`[or ,a ,b ,c] `[set ,a (or ,b ,c)])
        (`[xor ,a ,b ,c] `[set ,a (xor ,b ,c)])
        (`[nor ,a ,b ,c] `[set ,a (nor ,b ,c)])
        (`[slt ,a ,b ,c] `[set ,a (lt_s32 ,b ,c)])
        (`[sltu ,a ,b ,c] `[set ,a (lt_u32 ,b ,c)])
        (`[j ,a] `[begin_jump_to ,a])
        (`[jal ,a] `[begin (set (r 31) (+ pc 8)) (begin_jump_to ,a)])
        (`[beq ,a ,b ,c] `[if (= ,a ,b) (begin_jump_to ,c)])
        (`[bne ,a ,b ,c] `[if (!= ,a ,b) (begin_jump_to ,c)])
        (`[blez ,a ,b] `[if (lt_s32 ,a 0) (begin_jump_to ,b)])
        (`[bgtz ,a ,b] `[if (gt_s32 ,a 0) (begin_jump_to ,b)])
        (`[addi ,a ,b ,c] `(set ,a [add_s32 ,b ,c]))
        (`[addiu ,a ,b ,c] `(set ,a [add_u32 ,b ,c]))
        (`[andi ,a ,b ,c] `[set ,a (and ,b ,c)])
        (`[ori ,a ,b ,c] `[set ,a (or ,b ,c)])
        (`[xori ,a ,b ,c] `[set ,a (xor ,b ,c)])
        (`[lui ,a ,b] `(set ,a ,(arithmetic-shift b 16)))
        (`[mfc0 ,a ,b] `(set ,a ,b))
        (`[cfc0 ,a ,b] `(set ,a ,b))
        (`[mtc0 ,a ,b] `(set ,a ,b))
        (`[ctc0 ,a ,b] `(set ,a ,b))
        (`[lb ,a ,b] `(set ,a (ms8 ,b)))
        (`[lh ,a ,b] `(set ,a (ms16 ,b)))
        (`[lw ,a ,b] `(set ,a (mu32 ,b)))
        (`[lbu ,a ,b] `(set ,a (mu8 ,b)))
        (`[lhu ,a ,b] `(set ,a (mu16 ,b)))
        (`[sb ,a ,b] `(set (mu8 ,a) ,b))
        (`[sh ,a ,b] `(set (mu16 ,a) ,b))
        (`[sw ,a ,b] `(set (mu32 ,a) ,b))
        (`[not_implemented ,_ ,_ ,_ ,_ ,_, _] das)
    ))
