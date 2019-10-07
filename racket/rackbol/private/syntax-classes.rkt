#lang s-exp "lang.rkt"

(provide
    Storage$
    Alias$
    TABLE-Clause$
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
