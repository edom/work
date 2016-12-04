# Design principles

None of these principles are to be followed blindly.

None of these principles are permanent.

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

Focus more on the messaging than on the objects.
See [this email from Alan Kay](http://lists.squeakfoundation.org/pipermail/squeak-dev/1998-October/017019.html).

Think of objects like people communicating,
sending messages to each other,
asking others to do things.

## Java-specific principles

### Naming

Naming problem is a sign of design problem.

Use a thesaurus to help name your classes.

If you can't think of a name for a class,
perhaps the class shouldn't exist.

### Others

Don't be so focused on the classes that
you neglect the design of your packages.

Avoid static methods, except static factory methods.

Write at least two implementations before writing an interface.

Keep the interface small, but don't split
it into parts that are always used together.

Avoid getters and setters.
If the class is only responsible for structuring data, make the field public;
otherwise move the code using the field into the class.

Avoid checked exceptions.

Make classes final by default.
After you know how others will subclass it, make it non-final.
However, you may want to make Throwable subclasses non-final.

## Unfounded guesses

All maintainable software, regardless of language,
have high cohesion and low coupling.

Bugs are caused by what programmers don't know they don't know.
They make assumptions that they don't realize.

Write tests for what you know you don't know.

If you can't motivate yourself to write documentation,
think about showing off your work.
