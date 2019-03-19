/*

How the user thinks what something is.

The user's practical ontology.

*/

/** is_a(?Class, ?Type)

Type is #natural.
*/

/** has(?Class, ?N, ?Attribute)

Each instance of Class has N instances of Attribute.

N is 0, 1, or 0-1, many.
It is the cardinality.
*/

is_a(property(account,name), #natural).
has(account, 0-1, name).

entity(employee).
entity(department).

relationship(employee, department, 1:1)
