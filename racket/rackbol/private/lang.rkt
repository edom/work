#lang racket/base

(module reader syntax/module-reader rackbol/private/lang)

(require [except-in racket
    log
    λ
])
(provide [all-from-out racket])

(require [only-in "racket0/exim.rkt"
    require+provide/only
    require+provide/all
])
(provide
    require+provide/only
    require+provide/all
)

(require+provide/all syntax/parse)
(require+provide/all syntax/parse/define)

(require+provide/only
    [stc-racket/racket-extra
        λ LET LET* LETREC LETREC* GROUP deconstruct with-environment ->string
    ]
    [racket/include
        include
    ]
    [racket/pretty
        pretty-print
    ]
    ["racket0/alist.rkt"
        alist-ref
    ]
    ["racket0/date.rkt"
        current-date
        format-date/iso
    ]
    ["racket0/os.rkt"
        system**/exit-code
    ]
)
