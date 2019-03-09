type_definition(employee-id, #identifier).
type_definition(employee-name, #string).
type_definition(employee, #record([
    id : employee-id
    , name : employee-name
])).

type_maxbitcount(employee-id,64).
type_maxbytecount(employee-name,128).

webapp(employee).
