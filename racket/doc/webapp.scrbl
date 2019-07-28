#lang stc-racket/scribble-manual

@title{How should we develop Web applications?}


@section{Literature}

Erik Meijer has thought a lot about this.
Read his paper "Confessions of a Used Programming Language Salesman".
Relational-Object-XML (ROX) Triangle.


@section{Usage}

The STC Web Application Library presents various levels of abstractions.
The users can pick.
Combine.

Full-stack, compile to SQL and JavaScript?

Model from the business down to the infrastructure?
Doing too much?

@section{What is a web application?}

A web application functions like a desktop application,
but is instead hosted on the Internet for ease of deployment,
because there is only one copy of the web application,
compared to the many copies of desktop applications.
A web application programmer is more concerned with the ease of deployment than the linking of resources.
A web application can be thought of a misuse of the Web, not necessarily bad.

A server can be thought of as a function
whose domain is the set of all requests and
whose codomain is the set of all responses,
where "request" and "response" are defined
according to the protocol implemented by the server.
Indeed every server on the Internet can be thought of as such a function.
This view is not limited to web servers.

But that is a very low-level view of servers.

HTTP, URL, routing, and SQL, are some accidental complexities of web application programming.
The requirements are the essential complexity.
For example, in a tax calculator,
the tax law is the source of the software's essential complexity.
There is so much accidental complexity in web application programming.

The Web is more than HTTP.
The Web is about linking resources,
whereas HTTP is about transferring resources.


@section{Tool-maker point of view}


@subsection{Servlets}

@(require
    (for-label
        (only-in web-server/servlet-env
            serve/servlet
        )
    )
)

The bottom layer of this library is the @racketmodname[web-server/servlet-env] module that comes with Racket,
documented in @other-doc['(lib "web-server/scribblings/web-server.scrbl")].

Of course there are more layers below that,
such as the operating system, the network protocols, and so on,
but we blissfully ignore them, until they fail,
but most of the time they do their thankless jobs well,
and almost all software is built with the simplifying assumption that hardware is perfect.

A @emph{handler} is a function from Request to Response.

A @emph{request transformer} is a function from Request to Request.

A @emph{response transformer} is a function from Response to Response.


@subsection{Handler combinators}

A @emph{handler combinator} is a function from Handler to Handler.

Recall that Handler = (Request -> Response).

A page?
A handler/servlet?
A URL?
But how do we express that function?
We don't want if-else url checks?
Forms?

A handler combinator encompasses many things:
- rate-limited SRV
- authentication and authorization
- security
- analysis, tracking, logging
- circuit-breaker
- timeout-limit
- time limit, such as disabling some functionalities on Saturdays

limit-path : Path -> Handler -> Handler


@subsection{Handler algebra?}

Two web applications can be composed.
A + B.
But only if their paths do not overlap.
Several ways of combining A and B:
- If A 4xx or 5xx, try B.
- Balance A and B. Randomly, round robin, or request hash.


@subsection{TODO SQL}

See @other-doc['(lib "db/scribblings/db.scrbl")].

syntax/parse
(table RktId dbref schref)?

define-syntax SELECT
(SELECT (id name) FROM employee WHERE (= name "Jack") GROUP BY (id))


@subsection{TODO Styling}


@subsection{TODO JavaScript}

No need to accept both json and urlencoded?
Browser javascript today can send urlencoded?


@subsection{For people coming from Java}

Java interceptors/decorators = Racket higher-order functions

Racket "web-server" module is at a similar level of abstraction to Java Servlet API.


@subsection{Things not used?}

I was not going to use Racquel@fnurl{https://github.com/brown131/racquel} because its license was GPL3,
but then the author added the Apache2 and MIT licenses.@fnurl{https://github.com/brown131/racquel/issues/10}
I change my mind.
I should read the Racquel documentation.@fnurl{https://docs.racket-lang.org/racquel/index.html}


@section{Tool-user point of view}

Programmers think of a server as a function from Request to Response.

Users think of a server as a set of commands that users can tell the server to do.

Tool makers think of how a tool is made.
Tool users think of what they can do with a tool, and how they can use it.

Imagine if we could write like this to obtain a full web application:

@verbatim|{
(class employee)
(property employee name)

(class department)
(property department name)

(binary-relation 1 employee work-at 1 department)

(define user #f)

(define (get-user!)
    (if user
        user
        (begin
            (remember-user! (ask "Who are you"))
            user
        )
    )
)

(define (forget-user!)
    (set! user #f)
)

(define (remember-user! name)
    (set! user name)
    (tell (string-append "OK, from now on I will call you " name "."))
)

(command (tell-me 'where-does (employee e) 'work)
    (tell
        (string-append
            "Hi " (get-user!) ", "
            (if (binary-relate e work-at d)
                (string-append e " works at " d ".")
                (string-append "I don't know where " e " works.")
            )
        )
    )
)

(command (tell-me 'the-secret)
    (define user (get-user!))
    (tell
        (if (equal? user "fred")
            "The secret is 123."
            "Sorry, I can't tell you."
        )
    )
)
}|

Imagine that we speak at a higher level of abstraction,
about the interaction between man and machine,
not about HTTP requests and responses.
Imagine that we write web application like writing a console application.
Continuations are one of the ingredients.

ask queries the user.

tell informs/commands the user.


@section{Operations point of view}

After a web application is written,
it has to be hosted somewhere on the Internet (or an intranet), and maintained:
Disks have to be cleaned periodically,
because disk space has become so cheap that we take it for granted:
Most applications are built with the assumption that the disk always has enough free space.

Logs.

Backups.
