Monotypes form a ring, like the natural numbers.

This is an algebra of (mono)types.

Let A be a type.

Let B be a type.

Then A * B is a type (product).
- Associativity: (A * B) * C = A * (B * C).
- Commutativity: A * B = B * A.

Then A + B is a type (coproduct a.k.a. "untagged union").
- Associativity: (A + B) + C = A + (B + C).
- Commutativity: A + B = B + A.

Then A -> B is a type (exponential B^A).

There exists the zero type 0 (Void).
- 0 * A = 0.
- 0 + A = A.
- 0^A = 0.

There exists the unit type 1 (Unit).
- 1 * A = A.

Distributivity: A * (B + C) = A * B + A * C.

Additional properties not required by rings:
- Idempotence: A + A = A.
- Annihilation: A + Omega = Omega.
- Types almost form a [WP:Boolean algebra](https://en.wikipedia.org/wiki/Boolean_algebra) without negation.
