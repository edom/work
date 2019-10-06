#lang racket/base

(require
    (for-syntax
        racket/base
        racket/syntax
    )
    (except-in racket/base
        log
        λ
    )
    stc-racket/racket-extra
)

(provide
    (for-syntax
        (all-from-out
            racket/base
            racket/syntax
        )
    )
    (all-from-out
        racket/base
        stc-racket/racket-extra
    )
)
(require+provide/all
    syntax/parse
    syntax/parse/define
)
(require+provide/only
    [racket/include
        include
    ]
    [racket/pretty
        pretty-print
    ]
)
