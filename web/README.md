# Web Application Library

This library combines:

* Java 8
* Servlet API 3.1.0 (JSR 340)
* Java API for JSON Processing (JSR 353)
* jetty
* jsoup

## Git usage policy

The `master` branch contains experimental things,
but this branch should always be buildable.

Make a branch for each released version.

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
* Work with and build on existing codes and standards
instead of trying to replace them.
* It's better to have no abstraction than having a wrong abstraction.
* When in doubt, choose simplicity.
* Testing is not a substitute for programmer competence.
* If you need to use a debugger, the code is probably not simple enough.
* Progressive enhancement instead of graceful degradation.
* None of those principles are permanent.

## Design

The template is plain HTML file.
People can use web browser to view the template.
People only need to understand HTML to edit the template.

The server parses the HTML template into DOM, manipulates the DOM,
renders it back to HTML, and sends it to the client.

## Using the library

Clone this repository.

Run `gradle install` to install all artifacts to your local Maven repository.

In your `build.gradle` file, ensure that `mavenLocal()` is in your `repository`.

```
repositories {
    mavenLocal()
}
```

In your `build.gradle` file,
add the `compile` line to your project's `dependencies` block
for the artifacts you want to use.

```
dependencies {
    compile "com.spacetimecat:web:0.0.0-SNAPSHOT"
}
```

## Milestones for 0.0.0

### Unreached but likely

* request body
* response body
* CssFileInlining: copy the content of the file
pointed by link rel=stylesheet class=inline tag into the generated HTML
* static serving from classpath
* static serving from file system
* a distributable default style sheet
* documentation (javadoc)
* edit-and-refresh static files
* error page example
* static site example
* todo list example
* blog example
* address book example
* calendar example
* form example
* HTTP basic authentication example
* HTTP digest authentication example

### Unreached but questionable

* JDBC example
* JDBC CRUD example
* chat client-server example
* file download
* file upload
* multiple file upload
* redirect
* multilingual website example
* generate static website by spidering
* reverse proxy
* WebSocket
* single page app
* netty
* HTTP 2
* rate limiting
* caching, conditional GET, ETag
* Thymeleaf

### Reached

* JSON API example
* HTTP content negotiation (but this doesn't conform to the specification yet)
* gzip compression
* request logging

## Non-features

This library does not support TLS.
Use a reverse proxy capable of TLS termination.

## Legal matter

(c) 2016 Erik Dominikus

This project is licensed under the Apache License Version 2.0.
