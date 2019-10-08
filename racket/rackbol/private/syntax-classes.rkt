#lang s-exp "lang.rkt"

(provide
    Storage$
    Alias$
    TABLE-Clause$

    Possibly-Dotted-Id$
)

(define-syntax-class Storage$
    #:attributes (id virtual-connection-id)
    (pattern id:id
        #:with virtual-connection-id (format-id #'id "~a-virtual-connection" #'id)
    )
)

(define-splicing-syntax-class Alias$
    #:attributes (original alias)
    #:datum-literals (AS)
    (pattern [~seq original:id AS alias:id])
    (pattern [~seq original:id] #:with alias #'original)
)

(define-splicing-syntax-class TABLE-Clause$
    #:attributes (schema name alias [column 1] connection-id)
    #:datum-literals (TABLE)
    (pattern [~seq TABLE storage:Storage$ schema:id table:Alias$ (column-1:id column-2:id ...) ]
        #:with connection-id #'storage.virtual-connection-id
        #:with name #'table.original
        #:with alias #'table.alias
        #:with (column ...) #'(column-1 column-2 ...)
    ))

;;  --------------------

(define (split-dotted-id id)
    (define (make-id str) (string->identifier id str))
    (define str (identifier->string id))
    (match (string-split str "." #:trim? #f)
        [`(,suffix)         (cons #f (make-id suffix))]
        [`(,prefix ,suffix) (cons (make-id prefix) (make-id suffix))]
        [_                  #f]
    ))

(define-syntax-class Possibly-Dotted-Id$
    #:attributes (
        prefix      ;;  may be #f (but always present, so can't use ~?)
        suffix      ;;  required
    )
    (pattern id:id
        #:attr pair (split-dotted-id #'id)
        #:fail-unless (attribute pair) "expected an identifier that may contain a dot"
        #:attr prefix (car [attribute pair])
        #:attr suffix (cdr [attribute pair])
    )
)
