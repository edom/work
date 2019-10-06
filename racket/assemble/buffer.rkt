#lang racket/base

;;  mmap-backed executable buffers.

(require
    racket/format
    ffi/unsafe
)

(provide
    (all-from-out ffi/unsafe)

    (rename-out [getpagesize get-page-size])

    Buffer-ptr
    Buffer-limit
    Buffer-offset
    allocate-buffer
    free-buffer!
    Buffer-dump
    Buffer-reset!
    Buffer-read-byte
    Buffer-write-byte!
    Buffer-call

    format-ptr
)

;;  --------------------    mmap Constants.
;;
;;  These are taken from /usr/include/x86_64-linux-gnu/bits/mman-linux.h.

(define PROT_READ #x1)
(define PROT_WRITE #x2)
(define PROT_EXEC #x4)

(define MAP_SHARED #x1)
(define MAP_PRIVATE #x2)
(define MAP_ANONYMOUS #x20)
(define MAP_FAILED (cast -1 _sintptr _pointer))

;;  --------------------    Some C procedures.

(define _off _uintptr)

;;  This does not work.
;;  /usr/lib/x86_64-linux-gnu/libc.so is a linker script.
;;
;;(define LIBC "libc")

(define LIBC "/lib/x86_64-linux-gnu/libc.so.6")

(define-c mmap LIBC
    (_fun
        #:save-errno 'posix
        (addr : _pointer)
        (length : _size)
        (prot : _int)
        (flags : _int)
        (fd : _int)
        (offset : _off)
        ->
        _pointer
    ))

(define-c munmap LIBC (_fun #:save-errno 'posix (addr : _pointer) (length : _size) -> _int))
(define-c mprotect LIBC (_fun #:save-errno 'posix (addr : _pointer) (length : _size) (prot : _int) -> _int))
(define-c getpagesize LIBC (_fun -> _int))
(define-c strerror LIBC (_fun _int -> _string/locale))

;;  --------------------    Format.

(define PTR_BYTES (ctype-sizeof _pointer))

(define (format-ptr ptr)
    (format "0x~a"
        (~r (cast ptr _pointer _uintptr)
            #:base 16
            #:min-width (* 2 PTR_BYTES)
            #:pad-string "0")))

;;  --------------------    Buffer.

(struct Buffer (ptr limit offset) #:prefab #:mutable)

(define (Buffer-reset! buffer)
    (set-Buffer-offset! buffer 0)
)

(define (Buffer-dump buffer #:row-size [row-size 16])
    (when (< row-size 1)
        (error 'Buffer-dump "invalid row size: ~a" row-size))
    (define ptr (Buffer-ptr buffer))
    (define limit (Buffer-limit buffer))
    (define offset (Buffer-offset buffer))
    (printf "Dump of buffer ptr ~a limit ~a offset ~a:" (format-ptr ptr) limit offset)
    (for ([i (in-range limit)])
        (define byte (Buffer-read-byte buffer i))
        (when (= (modulo i row-size) 0)
            (printf "~n~a    " (~r i #:base 16 #:min-width 8 #:pad-string "0"))
        )
        (printf "~a " (~r byte #:base 16 #:min-width 2 #:pad-string "0"))
    )
    (newline)
)

(define
    (Buffer-read-byte buffer [offset (Buffer-offset buffer)])
    (define ptr (Buffer-ptr buffer))
    (define limit (Buffer-limit buffer))
    (unless (and (<= 0 offset) (< offset limit))
        (error 'Buffer-read-byte "offset out of bounds: ~a" offset))
    (ptr-ref ptr _uint8 offset)
)

(define
    (Buffer-write-byte! buffer byte
        #:at (offset [Buffer-offset buffer])
    )
    (define ptr (Buffer-ptr buffer))
    (define limit (Buffer-limit buffer))
    (when (>= offset limit)
        (error 'Buffer-write-byte! "buffer overrun"))
    (ptr-set! ptr _uint8 offset byte)
    (set-Buffer-offset! buffer (+ offset 1))
)

;;  Allocate executable memory.

(define (allocate-buffer limit)
    (define ptr (mmap #f limit
        (bitwise-ior PROT_READ PROT_WRITE PROT_EXEC)
        (bitwise-ior MAP_PRIVATE MAP_ANONYMOUS)
        -1  ;; fd
        0   ;; offset
        ))
    (when (equal? ptr MAP_FAILED)
        (error 'allocate-buffer "mmap failed: errno ~a: ~a"
            (saved-errno) (strerror (saved-errno))))
    (Buffer ptr limit 0)
)

(define (free-buffer! buffer)
    (define ptr (Buffer-ptr buffer))
    (define limit (Buffer-limit buffer))
    (unless (= (munmap ptr limit) 0)
        (printf "Warning: munmap failed: errno ~a: ~a~n"
            (saved-errno) (strerror (saved-errno)))
    )
    (set-Buffer-ptr! buffer #f)
    (set-Buffer-limit! buffer 0)
    (set-Buffer-offset! buffer 0)
)

;;  Dangerous operation.

(define (Buffer-call buffer cfuntype)
    (define proc (cast (Buffer-ptr buffer) _pointer cfuntype))
    (proc)
)
