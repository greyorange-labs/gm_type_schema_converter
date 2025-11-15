-module(gm_type_schema_converter_tests).

-include_lib("eunit/include/eunit.hrl").

%%%===================================================================
%%% Test Module for gm_type_schema_converter
%%%===================================================================

%%%===================================================================
%%% Test: capitalize_type_name/1
%%%===================================================================

capitalize_type_name_simple_test() ->
    ?assertEqual(<<"User">>, gm_type_schema_converter:capitalize_type_name(user)).

capitalize_type_name_with_underscore_test() ->
    ?assertEqual(<<"UserId">>, gm_type_schema_converter:capitalize_type_name(user_id)).

capitalize_type_name_multiple_underscores_test() ->
    ?assertEqual(<<"UserProfile">>, gm_type_schema_converter:capitalize_type_name(user_profile)).

capitalize_type_name_complex_test() ->
    ?assertEqual(<<"DebugLoggingEnableRequest">>, 
        gm_type_schema_converter:capitalize_type_name(debug_logging_enable_request)).

%%%===================================================================
%%% Test: type_to_schema/2 - Primitive Types
%%%===================================================================

type_to_schema_binary_test() ->
    TypeDef = {my_string, {type, 1, binary, []}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(#{<<"type">> => <<"string">>}, Schema).

type_to_schema_integer_test() ->
    TypeDef = {my_int, {type, 1, integer, []}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(#{<<"type">> => <<"integer">>}, Schema).

type_to_schema_float_test() ->
    TypeDef = {my_float, {type, 1, float, []}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(#{<<"type">> => <<"number">>}, Schema).

type_to_schema_boolean_test() ->
    TypeDef = {my_bool, {type, 1, boolean, []}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(#{<<"type">> => <<"boolean">>}, Schema).

%%%===================================================================
%%% Test: type_to_schema/2 - Map Types
%%%===================================================================

type_to_schema_map_all_required_test() ->
    TypeDef = {person,
        {type, 1, map, [
            {type, 1, map_field_exact, [{atom, 1, name}, {type, 1, binary, []}]},
            {type, 1, map_field_exact, [{atom, 1, age}, {type, 1, integer, []}]}
        ]}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(<<"object">>, maps:get(<<"type">>, Schema)),
    Properties = maps:get(<<"properties">>, Schema),
    ?assertMatch(#{<<"name">> := #{<<"type">> := <<"string">>}}, Properties),
    ?assertMatch(#{<<"age">> := #{<<"type">> := <<"integer">>}}, Properties),
    Required = maps:get(<<"required">>, Schema),
    ?assert(lists:member(<<"name">>, Required)),
    ?assert(lists:member(<<"age">>, Required)).

type_to_schema_map_with_optional_test() ->
    TypeDef = {user,
        {type, 1, map, [
            {type, 1, map_field_exact, [{atom, 1, name}, {type, 1, binary, []}]},
            {type, 1, map_field_assoc, [{atom, 1, email}, {type, 1, binary, []}]}
        ]}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(<<"object">>, maps:get(<<"type">>, Schema)),
    Required = maps:get(<<"required">>, Schema),
    ?assert(lists:member(<<"name">>, Required)),
    ?assertNot(lists:member(<<"email">>, Required)).

type_to_schema_map_empty_test() ->
    TypeDef = {empty_map, {type, 1, map, any}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(#{<<"type">> => <<"object">>}, Schema).

%%%===================================================================
%%% Test: type_to_schema/2 - List Types
%%%===================================================================

type_to_schema_list_test() ->
    TypeDef = {user_list, {type, 1, list, [{type, 1, binary, []}]}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(<<"array">>, maps:get(<<"type">>, Schema)),
    Items = maps:get(<<"items">>, Schema),
    ?assertEqual(#{<<"type">> => <<"string">>}, Items).

%%%===================================================================
%%% Test: type_to_schema/2 - Union Types
%%%===================================================================

type_to_schema_union_test() ->
    TypeDef = {status,
        {type, 1, union, [
            {atom, 1, active},
            {atom, 1, inactive}
        ]}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertMatch(#{<<"oneOf">> := _}, Schema),
    OneOf = maps:get(<<"oneOf">>, Schema),
    ?assertEqual(2, length(OneOf)).

%%%===================================================================
%%% Test: type_to_schema/2 - User Type References
%%%===================================================================

type_to_schema_user_type_reference_test() ->
    %% Define a base type
    BaseType = {user_id, {type, 1, binary, []}},
    %% Reference it
    TypeDef = {ref_type, {user_type, 1, user_id, []}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, [BaseType]),
    ?assertMatch(#{<<"$ref">> := <<"#/components/schemas/UserId">>}, Schema).

type_to_schema_circular_reference_test() ->
    %% Define a type that references itself
    NodeType = {node,
        {type, 1, map, [
            {type, 1, map_field_exact, [{atom, 1, value}, {type, 1, binary, []}]},
            {type, 1, map_field_exact, [{atom, 1, children}, {type, 1, list, [{user_type, 1, node, []}]}]}
        ]}},
    Schema = gm_type_schema_converter:type_to_schema(NodeType, [NodeType]),
    ?assertEqual(<<"object">>, maps:get(<<"type">>, Schema)),
    Properties = maps:get(<<"properties">>, Schema),
    ChildrenSchema = maps:get(<<"children">>, Properties),
    ?assertEqual(<<"array">>, maps:get(<<"type">>, ChildrenSchema)),
    ItemsSchema = maps:get(<<"items">>, ChildrenSchema),
    ?assertMatch(#{<<"$ref">> := <<"#/components/schemas/Node">>}, ItemsSchema).

%%%===================================================================
%%% Test: types_to_schemas/1
%%%===================================================================

types_to_schemas_single_type_test() ->
    Types = [{my_string, {type, 1, binary, []}}],
    Schemas = gm_type_schema_converter:types_to_schemas(Types),
    ?assertMatch(#{<<"MyString">> := _}, Schemas),
    MyStringSchema = maps:get(<<"MyString">>, Schemas),
    ?assertEqual(#{<<"type">> => <<"string">>}, MyStringSchema).

types_to_schemas_multiple_types_test() ->
    Types = [
        {user_id, {type, 1, binary, []}},
        {user_name, {type, 1, binary, []}},
        {user_age, {type, 1, integer, []}}
    ],
    Schemas = gm_type_schema_converter:types_to_schemas(Types),
    ?assertMatch(#{<<"UserId">> := _}, Schemas),
    ?assertMatch(#{<<"UserName">> := _}, Schemas),
    ?assertMatch(#{<<"UserAge">> := _}, Schemas),
    ?assertEqual(3, map_size(Schemas)).

types_to_schemas_with_references_test() ->
    Types = [
        {user_id, {type, 1, binary, []}},
        {user,
            {type, 1, map, [
                {type, 1, map_field_exact, [{atom, 1, id}, {user_type, 1, user_id, []}]},
                {type, 1, map_field_exact, [{atom, 1, name}, {type, 1, binary, []}]}
            ]}}
    ],
    Schemas = gm_type_schema_converter:types_to_schemas(Types),
    ?assertMatch(#{<<"UserId">> := _}, Schemas),
    ?assertMatch(#{<<"User">> := _}, Schemas),
    UserSchema = maps:get(<<"User">>, Schemas),
    Properties = maps:get(<<"properties">>, UserSchema),
    IdSchema = maps:get(<<"id">>, Properties),
    ?assertMatch(#{<<"$ref">> := <<"#/components/schemas/UserId">>}, IdSchema).

%%%===================================================================
%%% Test: Complex Real-World Scenarios
%%%===================================================================

types_to_schemas_diagnose_infra_request_test() ->
    %% Test the actual type from gm_common_http_handler
    Types = [
        {infra_test_name,
            {type, 1, union, [
                {atom, 1, process_registration},
                {atom, 1, influx},
                {atom, 1, kafka},
                {atom, 1, elastic},
                {atom, 1, postgres},
                {atom, 1, all},
                {atom, 1, mnesia},
                {atom, 1, emqx},
                {atom, 1, rabbitmq},
                {atom, 1, opc_client},
                {atom, 1, gen_saga},
                {atom, 1, auth_service},
                {atom, 1, lager}
            ]}},
        {diagnose_infra_request,
            {type, 1, map, [
                {type, 1, map_field_exact, [{atom, 1, tests}, {type, 1, list, [{user_type, 1, infra_test_name, []}]}]}
            ]}}
    ],
    Schemas = gm_type_schema_converter:types_to_schemas(Types),
    ?assertMatch(#{<<"InfraTestName">> := _}, Schemas),
    ?assertMatch(#{<<"DiagnoseInfraRequest">> := _}, Schemas),
    
    %% Check DiagnoseInfraRequest schema
    RequestSchema = maps:get(<<"DiagnoseInfraRequest">>, Schemas),
    ?assertEqual(<<"object">>, maps:get(<<"type">>, RequestSchema)),
    Properties = maps:get(<<"properties">>, RequestSchema),
    TestsSchema = maps:get(<<"tests">>, Properties),
    ?assertEqual(<<"array">>, maps:get(<<"type">>, TestsSchema)),
    ItemsSchema = maps:get(<<"items">>, TestsSchema),
    ?assertMatch(#{<<"$ref">> := <<"#/components/schemas/InfraTestName">>}, ItemsSchema),
    
    %% Check required field
    Required = maps:get(<<"required">>, RequestSchema),
    ?assert(lists:member(<<"tests">>, Required)).

types_to_schemas_nested_map_test() ->
    Types = [
        {address,
            {type, 1, map, [
                {type, 1, map_field_exact, [{atom, 1, street}, {type, 1, binary, []}]},
                {type, 1, map_field_exact, [{atom, 1, city}, {type, 1, binary, []}]}
            ]}},
        {user,
            {type, 1, map, [
                {type, 1, map_field_exact, [{atom, 1, name}, {type, 1, binary, []}]},
                {type, 1, map_field_assoc, [{atom, 1, address}, {user_type, 1, address, []}]}
            ]}}
    ],
    Schemas = gm_type_schema_converter:types_to_schemas(Types),
    ?assertMatch(#{<<"Address">> := _}, Schemas),
    ?assertMatch(#{<<"User">> := _}, Schemas),
    
    UserSchema = maps:get(<<"User">>, Schemas),
    Properties = maps:get(<<"properties">>, UserSchema),
    AddressRef = maps:get(<<"address">>, Properties),
    ?assertMatch(#{<<"$ref">> := <<"#/components/schemas/Address">>}, AddressRef).

%%%===================================================================
%%% Test: Edge Cases
%%%===================================================================

type_to_schema_unknown_type_test() ->
    %% Unknown type should return object as fallback
    TypeDef = {unknown, {type, 1, unknown_type, []}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(#{<<"type">> => <<"object">>}, Schema).

types_to_schemas_empty_list_test() ->
    Schemas = gm_type_schema_converter:types_to_schemas([]),
    ?assertEqual(#{}, Schemas).

capitalize_type_name_empty_atom_test() ->
    %% Edge case: empty atom (shouldn't happen in practice)
    Result = gm_type_schema_converter:capitalize_type_name(''),
    ?assert(is_binary(Result)).

