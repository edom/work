#lang s-exp "base.rkt"

(require
    (for-syntax
        "sql.rkt"
    )
)

(provide
    (for-syntax
        SELECT$
    )
    SELECT
    (struct-out Sql-Select)
)

(begin-for-syntax

    (define (binary op a b)
        (~a "(" a " " op " " b ")")
    )

    (define-syntax-class Binary
        #:datum-literals (= != < <= > >= + - * /)
        (pattern =)
        (pattern !=)
        (pattern <)
        (pattern <=)
        (pattern >)
        (pattern >=)
        (pattern +)
        (pattern -)
        (pattern *)
        (pattern /)
    )

    ;;  "render" is a syntax object that is passed to syntax-local-eval
    ;;  in order to produce a string that is the SQL statement.
    ;;
    ;;  Is there a better way to do this?
    ;;  Perhaps "render" should be a transformer instead of an attribute?

    (define-splicing-syntax-class Expr
        #:attributes (render)
        #:datum-literals (AND OR NOT IN NOT-IN IS-NULL IS-NOT-NULL)
        (pattern value:number
            #:attr render #'(number->string value)
        )
        (pattern value:string
            #:attr render #'(sql-escape-string value)
        )
        ;;  We assume that each table and each column has no dots in their name.
        (pattern alias:Possibly-Dotted-Id$
            #:attr table-alias (attribute alias.prefix)
            #:attr column-alias (attribute alias.suffix)
            #:attr render #'(string-append
                (~?
                    (~@ (sql-escape-id 'table-alias) ".")
                    (~@)
                )
                (sql-escape-id 'column-alias)
            )
        )
        (pattern (op:Binary a:Expr b:Expr)
            #:attr render #'(binary 'op a.render b.render)
        )
        (pattern (IS-NULL a:Expr)
            #:attr render #'(string-append "(" a.render " IS NULL)")
        )
        (pattern (IS-NOT-NULL a:Expr)
            #:attr render #'(string-append "(" a.render " IS NOT NULL)")
        )
        (pattern (IN a:Expr [b:Expr ...])
            #:attr render #'(string-append "(" a.render " IN ("
                (~a b.render ... #:separator ", ")
            "))")
        )
        (pattern (NOT-IN a:Expr [b:Expr ...])
            #:attr render #'(string-append "(" a.render " NOT IN ("
                (~a b.render ... #:separator ", ")
            "))")
        )
        (pattern (AND a:Expr b:Expr ...)
            #:attr render #'(string-append "(" a.render (~@ " AND " b.render) ... ")")
        )
        (pattern (OR a:Expr b:Expr ...)
            #:attr render #'(string-append "(" a.render (~@ " OR " b.render) ... ")")
        )
        (pattern (NOT a:Expr)
            #:attr render #'(string-append "(NOT " a.render ")")
        )
    )

    (define-splicing-syntax-class Column
        #:datum-literals (AS)
        (pattern name:Sql-Id$
            #:with alias #'name
            #:with render #'name.render
        )
        (pattern (name:Sql-Id$ AS alias:Sql-Id$)
            #:with render #'(~a name.render " AS " alias.render)
        )
    )

    (define-splicing-syntax-class Column-Seq
        #:attributes (aliases render)
        (pattern (~seq column-1:Column column-2:Column ...)
            #:with aliases #'(column-1.alias column-2.alias ...)
            #:with render #'(~a
                (~@ column-1.render)
                (~@ column-2.render) ...
                #:separator ", "
            )
        )
    )

    (define-syntax-class Sql-Id$
        #:datum-literals (FROM WHERE LEFT RIGHT JOIN)
        #:attributes (render)
        (pattern (~and-not id:id (~or* FROM WHERE LEFT RIGHT JOIN))
            #:with render #'(sql-escape-id 'id)
        ))

    (define-syntax-class Sql-Qual-Id$
        #:datum-literals (FROM WHERE LEFT RIGHT JOIN)
        #:attributes (prefix suffix render)
        (pattern (~and-not id:Possibly-Dotted-Id$ (~or* FROM WHERE LEFT RIGHT JOIN))
            #:attr prefix (attribute id.prefix)
            #:attr suffix (attribute id.suffix)
            #:with render #'(string-append
                (~? (~@ (sql-escape-id 'prefix) ".") (~@))
                (sql-escape-id 'suffix)
            )
        ))

    (define-splicing-syntax-class From$
        #:datum-literals (AS)
        #:attributes (schema table alias render)
        (pattern (schema:Sql-Id$ table:Sql-Id$)
            #:attr alias #f
            #:with render #'(string-append schema.render "." table.render)
        )
        (pattern (schema:Sql-Id$ table:Sql-Id$ AS alias:Sql-Id$)
            #:with render #'(string-append schema.render "." table.render " AS " alias.render)
        )
        (pattern (~seq
                id:Sql-Qual-Id$
                (~optional-seq AS alias:Sql-Id$)
            )
            #:attr schema (attribute id.prefix)
            #:attr table (attribute id.suffix)
            #:with render #'(string-append
                id.render
                (~? (~@ " AS " alias.render) (~@))
            )
        )
    )

    (define-syntax-class SELECT$
        #:attributes (column-aliases string)
        #:datum-literals (FROM WHERE)
        [pattern
            (_  columns:Column-Seq
                FROM from:From$
                (~optional-seq WHERE where:Expr)
            )
            #:with column-aliases #'columns.aliases
            #:attr string (datum->syntax #'SELECT$
                (syntax-local-eval
                    #'(~a
                        'SELECT columns.render
                        'FROM from.render
                        (~? (string-append "WHERE " where.render) (~@))
                        #:separator " "
                    )
                )
            )
        ]
    )
)

(struct Sql-Select (
        var-ids     ;;  list of identifier syntax objects
        string      ;;  SQL
    )
    #:prefab
)

(define-syntax-parser SELECT
    [   q:SELECT$
        #'(Sql-Select 'q.column-aliases q.string)
    ])
