% -------------------- connectors

connector_pins(user_defined(Name), Pins) :-
    connector_pins(Name, Pins).

connector_pins(state,[
    state/1
    , state_name/2
    , state_type/2
    , state_initializer/2
]).

connector_pins(globalization,[
    term_locale_string/3
]).

connector_pins(type,[
    type_definition/2
    , type_maxbitcount/2
    , type_maxbytecount/2
]).

connector_pins(procedure,[
    procedure/1
    , procedure_name/2
    , procedure_input/3
    , procedure_check/2
    , procedure_output/2
]).

connector_pins(page,[
    page/1
    , page_name/2
    , page_method/2
    , page_path/2
    , page_content/2
]).

connector_pins(java_program_model,[
    element_access/2
    , element_final/2
    , element_static/2
    , element_annotation/3

    , class/1
    , class_package_name/2
    , class_name/2
    , class_extend/2
    , class_implement/2
    , class_implements/2
    , class_constructor/2
    , class_field/2
    , class_method/2

    , field/1
    , field_initializer/2
    , field_name/2
    , field_type/2

    , callable_parameter/3
    , callable_statement/3
    , callable_throw/2
    , callable_throws/2
    , constructor/1
    , method/1
    , method_name/2
    , method_return_type/2
    , parameter_name/2
    , parameter_type/2
]).

connector_pins(system_type_for_java,[
    recordtype/1
    , recordtype_field/4
    , type_integer_bit/2
    , type_identifier_bit/2
    , type_string/1
    , type_optional/2
]).

connector_pins(imp_database,[
    database/1
    , database_name/2
    , database_host/2
    , database_port/2
    , database_catalog/2
    , database_username/2
    , database_password/2
]).

connector_pins(imp_java_writer,[
    base_package_name/1
    , maven_coordinates/3
    , output_dir/1
    , dry_run/1
]).

schema_type_connector(model, plug, user_defined(state)).
schema_type_connector(model, plug, globalization).
schema_type_connector(model, plug, type).
schema_type_connector(model, plug, procedure).
schema_type_connector(model, plug, user_defined(page)).
schema_type_connector(model, plug, imp_database).
schema_type_connector(model, plug, imp_java_writer).
schema_type_connector(system, socket, user_defined(state)).
schema_type_connector(system, socket, procedure).
schema_type_connector(system, socket, globalization).
schema_type_connector(system, socket, type).
schema_type_connector(system, socket, procedure).
schema_type_connector(system, plug, state).
schema_type_connector(system, plug, system_type_for_java).
schema_type_connector(webapp, socket, state).
schema_type_connector(webapp, socket, user_defined(page)).
schema_type_connector(webapp, plug, page).
schema_type_connector(webapp_javaprogram, socket, state).
schema_type_connector(webapp_javaprogram, socket, page).
schema_type_connector(webapp_javaprogram, socket, imp_database).
schema_type_connector(webapp_javaprogram, socket, page).
schema_type_connector(webapp_javaprogram, socket, system_type_for_java).
schema_type_connector(webapp_javaprogram, socket, imp_java_writer).
schema_type_connector(webapp_javaprogram, plug, java_program_model).
schema_type_connector(java_program, socket, imp_java_writer).
schema_type_connector(java_program, socket, java_program_model).
schema_type_connector(java_writer, socket, java_program_model).
schema_type_connector(java_writer, socket, imp_java_writer).

% -------------------- schemas

schema_plug_pins(Schema, Connector, Pins) :-
    schema_type_connector(Schema, plug, Connector),
    connector_pins(Connector, Pins).

schema_socket_pins(Schema, Connector, Pins) :-
    schema_type_connector(Schema, socket, Connector),
    connector_pins(Connector, Pins).

schema_definition(model,[]).
schema_definition(system,[file-"schema/system.pro"]).
schema_definition(webapp,[file-"schema/web_application.pro"]).
schema_definition(webapp_javaprogram,[]).
schema_definition(java_program,[file-"schema/java_program.pro"]).
schema_definition(java_writer,[]).

% -------------------- schema connections

/** schema_connection(?Plug,?Socket) is nondet.

Each of Plug and Socket is a Module:Name term.
*/

schema_connection(A:C, B:C) :- A \= B.
