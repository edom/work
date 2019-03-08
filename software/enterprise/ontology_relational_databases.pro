:- module(ontology_relational_databases,[]).
/** <module> Ontology for relational databases

An instance of this ontology describes relational databases.
*/

/** database(?DatabaseId) is nondet.
    database_catalog(?DatabaseId,?CatalogId) is nondet.
    catalog_schema(?CatalogId,?SchemaId) is nondet.
    catalog_name(?CatalogId,?Atom) is nondet.
    schema(?SchemaId) is nondet.
    schema_name(?SchemaId,?Atom) is nondet.
    table(?TableId) is nondet.
    table_name(?TableId,?Atom) is nondet.
    column(?ColId) is nondet.
    column_name(?ColId,?Atom) is nondet.
    column_type(?ColId,?ColType) is nondet.
    column_nullable(?ColId,?Boolean) is nondet.
*/

:- multifile
    database/1,
    database_catalog/2,
    catalog_schema/2,
    catalog_name/2,
    schema/1,
    schema_name/2,
    table/1,
    table_name/2,
    column/1,
    column_name/2,
    column_type/2,
    column_nullable/2.
