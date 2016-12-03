# Design principles

Use Semantic Versioning.

Don't invert control.
The user defines the main class and calls the library,
not the other way around.

Minimize magic.
This is so that everyone can use an IDE
to browse and understand the code.
This means avoiding annotations and reflection.

Work with existing codes.

When in doubt, choose simplicity

Progressive enhancement instead of graceful degradation.

If a class is hard to name, perhaps it shouldn't exist.

Focus more on the messaging than on the objects.
See [this email from Alan Kay](http://lists.squeakfoundation.org/pipermail/squeak-dev/1998-October/017019.html).

None of these principles are to be followed blindly.

None of these principles are permanent.
