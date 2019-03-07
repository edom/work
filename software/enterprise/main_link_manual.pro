/** <module> Manual explicit linking of multifile predicates

This is a separate file because this has to be loaded after all modules have been loaded.
This has to be loaded using consult/1 and not include/1.
*/

java_write:java_field(A,B,C) :- java:field_name(A,B), java:field_type(A,C).

translation_java:recordtype(A) :- spec:recordtype(A).
translation_java:recordtype_field(A,B) :- spec:recordtype_field(A,B).
translation_java:type_field_name(A,B) :- spec:field_name(A,B).
translation_java:type_field_type(A,B) :- spec:field_type(A,B).
