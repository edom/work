# Web Application Library

This library combines:

* Java 8
* Servlet API 3.1.0 (JSR 340)
* jetty
* jsoup

## Design principles

* Follow the Semantic Versioning standard.
* It's not all-or-none.
Let the user pick the parts he wants to use.
* Don't invert control.
The user defines the main class and calls the library,
not the other way around.
* Minimize magic so that everyone can use an IDE
to browse and understand the code.
This means avoiding annotations and reflection.
* It's better to have no abstraction than having a wrong abstraction.
* When in doubt, choose simplicity.
* Testing is not a substitute for programmer competence.
* If you need to use a debugger, the code is probably not simple enough.

## Design

* The server manipulates HTML DOM.

## Legal matter

(c) 2016 Erik Dominikus

This project is licensed under the Apache License Version 2.0.
