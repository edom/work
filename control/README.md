Use your Java IDE to control your system (build, deploy, provide, configure,
automate, monitor, test, whatever you can think of) instead of opening multiple
terminal windows to build and run your servers.

This aims to be like a combination of shell, make, Ansible, and Gradle, but:

* This uses *static typing*.  The compiler helps you catch mistakes.
* This uses *the same language* that you use to write your other programs.
  Your IDE can help you write, check, and understand your configurations
without disrupting your flow too much.

We use plain Java for configurations with a domain-specific approach.
It emulates English somewhat as far as Java syntax allows.

The question to answer before you use Ansible is: Why abuse YAML if you
eventually need variables, conditionals, and repetitions?  Why don't you just
use a Turing-complete language?  Why abuse a *data serialization language* and
a string templating library instead of just using a *programming language* if
the latter is what you really need?

If users abuse the power of Turing-complete languages, it's their problem.
We provide mechanism, not policy,
and it's their responsibility to restrain themselves.

This project is licensed under the 3-clause BSD license.
