#lang racket

;;  Generalized getters and setters.

(require
    (for-syntax
        (only-in racket/syntax
            format-id
        )
    )
)
(provide
    (struct-out Struct)
    (struct-out Field)
    *struct-info-hash*
    (rename-out
        (my-define-struct define-struct-2)
    )
    get
)

;;  --------------------    Keep track of struct definitions,
;;  because Racket doesn't keep track of field names.

(struct Struct (name fields) #:transparent)

(struct Field (
        name    ;;  Symbol
        get     ;;  Object Property -> Value
        set!    ;;  Object Property Value -> void (or should we return Object instead?)
    ) #:transparent
)

;;  This should be accessed only in phase 0,
;;  so that the information is available at runtime to normal code.
(define *struct-info-hash* (make-hash))

;;  (getter Struct Field)
(define-syntax (getter stx)
    (with-syntax (
            ((_getter Context Struct_Id Field_Id) stx)
        )
        (format-id #'Context "~a-~a" (syntax-e #'Struct_Id) (syntax-e #'Field_Id) #:source stx)
    )
)

;;  (my-define-struct Name Fields Option ...)
(define-syntax (my-define-struct stx)
    (syntax-case stx ()
        ((_ SName FNames Option ...)
            (with-syntax (
                    ((FName ...) #'FNames)
                )
                #'(begin
                    ;;  We can't use #:property to embed our annotation,
                    ;;  because Option may have #:prefab.
                    (struct SName FNames Option ...)
                    (let* (
                            (field_infos (make-immutable-hash
                                (list
                                    (let* (
                                            (getter_proc (getter SName SName FName))
                                            (finfo (Field 'FName getter_proc 'set))
                                        )
                                        (cons 'FName finfo)
                                    )
                                    ...
                                )
                            ))
                            (struct_info (Struct 'SName field_infos))
                        )
                        (hash-set! *struct-info-hash* 'SName struct_info)
                    )
                )
            )
        )
    )
)

;;  --------------------    Generalized getter.

(define (get object property)
    (cond
        ((struct? object)
            (define-values (type _skipped?0) (struct-info object))
            (define-values (
                    struct-name
                    _init-field-cnt
                    _auto-field-cnt
                    _accessor-proc
                    _mutator-proc
                    _immutable-k-list
                    _super-type
                    _skipped?1
                )
                (struct-type-info type)
            )
            (define my (hash-ref *struct-info-hash* struct-name))
            (define field (hash-ref (Struct-fields my) property
                (lambda ()
                    (error 'get "struct \"~a\" has no field named \"~a\"" struct-name property)
                )
            ))
            (define get (Field-get field))
            (get object)
        )
        ((hash? object)
            (hash-ref object property)
        )
        ((vector? object)
            (vector-ref object property)
        )
        (else
            (error (format "get: object ~v does not have property ~v" object property))
        )
    )
)
