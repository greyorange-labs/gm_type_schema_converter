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

%%%===================================================================
%%% Test: Integer Range (Native Erlang Syntax) - Phase 0
%%%===================================================================

integer_range_test() ->
    TypeDef = {age, {type, 0, range, [{integer, 0, 0}, {integer, 0, 150}]}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(<<"integer">>, maps:get(<<"type">>, Schema)),
    ?assertEqual(0, maps:get(<<"minimum">>, Schema)),
    ?assertEqual(150, maps:get(<<"maximum">>, Schema)).

integer_range_port_test() ->
    TypeDef = {port, {type, 0, range, [{integer, 0, 1024}, {integer, 0, 65535}]}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(<<"integer">>, maps:get(<<"type">>, Schema)),
    ?assertEqual(1024, maps:get(<<"minimum">>, Schema)),
    ?assertEqual(65535, maps:get(<<"maximum">>, Schema)).

%%%===================================================================
%%% Test: Float Constraints (Wrapper Syntax) - Phase 1
%%%===================================================================

float_constraints_test() ->
    TypeDef = {price,
        {type, 0, tuple, [
            {atom, 0, float_with_constraints},
            {type, 0, map, [
                {type, 0, map_field_assoc, [{atom, 0, minimum}, {float, 0, 0.0}]},
                {type, 0, map_field_assoc, [{atom, 0, maximum}, {float, 0, 1000.0}]},
                {type, 0, map_field_assoc, [{atom, 0, multiple_of}, {float, 0, 0.01}]}
            ]}
        ]}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(<<"number">>, maps:get(<<"type">>, Schema)),
    ?assertEqual(0.0, maps:get(<<"minimum">>, Schema)),
    ?assertEqual(1000.0, maps:get(<<"maximum">>, Schema)),
    ?assertEqual(0.01, maps:get(<<"multipleOf">>, Schema)).

float_constraints_exclusive_test() ->
    TypeDef = {positive_float,
        {type, 0, tuple, [
            {atom, 0, float_with_constraints},
            {type, 0, map, [
                {type, 0, map_field_assoc, [{atom, 0, exclusive_minimum}, {atom, 0, true}]}
            ]}
        ]}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(<<"number">>, maps:get(<<"type">>, Schema)),
    ?assertEqual(true, maps:get(<<"exclusiveMinimum">>, Schema)).

%%%===================================================================
%%% Test: String Constraints (Wrapper Syntax) - Phase 1
%%%===================================================================

string_constraints_test() ->
    TypeDef = {username,
        {type, 0, tuple, [
            {atom, 0, binary_with_constraints},
            {type, 0, map, [
                {type, 0, map_field_assoc, [{atom, 0, min_length}, {integer, 0, 3}]},
                {type, 0, map_field_assoc, [{atom, 0, max_length}, {integer, 0, 32}]},
                {type, 0, map_field_assoc, [{atom, 0, pattern}, {bin, 0, [{bin_element, 0, {string, 0, "^[a-zA-Z0-9_-]+$"}, default, default}]}]}
            ]}
        ]}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(<<"string">>, maps:get(<<"type">>, Schema)),
    ?assertEqual(3, maps:get(<<"minLength">>, Schema)),
    ?assertEqual(32, maps:get(<<"maxLength">>, Schema)),
    ?assertEqual(<<"^[a-zA-Z0-9_-]+$">>, maps:get(<<"pattern">>, Schema)).

string_constraints_format_test() ->
    TypeDef = {email,
        {type, 0, tuple, [
            {atom, 0, binary_with_constraints},
            {type, 0, map, [
                {type, 0, map_field_assoc, [{atom, 0, format}, {bin, 0, [{bin_element, 0, {string, 0, "email"}, default, default}]}]}
            ]}
        ]}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(<<"string">>, maps:get(<<"type">>, Schema)),
    ?assertEqual(<<"email">>, maps:get(<<"format">>, Schema)).

%%%===================================================================
%%% Test: Array Constraints (Wrapper Syntax) - Phase 1
%%%===================================================================

array_constraints_test() ->
    TypeDef = {tags,
        {type, 0, tuple, [
            {atom, 0, list_with_constraints},
            {type, 0, binary, []},
            {type, 0, map, [
                {type, 0, map_field_assoc, [{atom, 0, min_items}, {integer, 0, 1}]},
                {type, 0, map_field_assoc, [{atom, 0, max_items}, {integer, 0, 20}]},
                {type, 0, map_field_assoc, [{atom, 0, unique_items}, {atom, 0, true}]}
            ]}
        ]}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(<<"array">>, maps:get(<<"type">>, Schema)),
    ?assertEqual(#{<<"type">> => <<"string">>}, maps:get(<<"items">>, Schema)),
    ?assertEqual(1, maps:get(<<"minItems">>, Schema)),
    ?assertEqual(20, maps:get(<<"maxItems">>, Schema)),
    ?assertEqual(true, maps:get(<<"uniqueItems">>, Schema)).

%%%===================================================================
%%% Test: Object Constraints (Wrapper Syntax) - Phase 1
%%%===================================================================

object_constraints_test() ->
    TypeDef = {config,
        {type, 0, tuple, [
            {atom, 0, map_with_constraints},
            {type, 0, map, [
                {type, 0, map_field_assoc, [{atom, 0, min_properties}, {integer, 0, 1}]},
                {type, 0, map_field_assoc, [{atom, 0, max_properties}, {integer, 0, 10}]}
            ]},
            {type, 0, map, [
                {type, 0, map_field_assoc, [{atom, 0, key1}, {type, 0, binary, []}]},
                {type, 0, map_field_assoc, [{atom, 0, key2}, {type, 0, integer, []}]}
            ]}
        ]}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(<<"object">>, maps:get(<<"type">>, Schema)),
    ?assertEqual(1, maps:get(<<"minProperties">>, Schema)),
    ?assertEqual(10, maps:get(<<"maxProperties">>, Schema)),
    ?assertMatch(#{<<"key1">> := _}, maps:get(<<"properties">>, Schema)),
    ?assertMatch(#{<<"key2">> := _}, maps:get(<<"properties">>, Schema)).

%%%===================================================================
%%% Test: Composition Keywords (allOf, not) - Phase 1
%%%===================================================================

all_of_required_test() ->
    TypeDef = {complete_profile,
        {type, 0, tuple, [
            {atom, 0, all_of_required},
            {tuple, 0, [{atom, 0, name}, {atom, 0, email}]},
            {type, 0, map, [
                {type, 0, map_field_assoc, [{atom, 0, name}, {type, 0, binary, []}]},
                {type, 0, map_field_assoc, [{atom, 0, email}, {type, 0, binary, []}]},
                {type, 0, map_field_assoc, [{atom, 0, phone}, {type, 0, binary, []}]}
            ]}
        ]}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(<<"object">>, maps:get(<<"type">>, Schema)),
    ?assertMatch(#{<<"allOf">> := _}, Schema),
    AllOf = maps:get(<<"allOf">>, Schema),
    ?assertEqual(2, length(AllOf)),
    %% Check that each allOf entry requires one of the keys
    [First | _] = AllOf,
    ?assertMatch(#{<<"required">> := _}, First).

not_constraint_test() ->
    %% Test not constraint - string that is not empty
    TypeDef = {non_empty_string,
        {type, 0, tuple, [
            {atom, 0, not_constraint},
            {type, 0, binary, []},
            %% Negated schema: empty string enum
            {type, 0, map, [
                {type, 0, map_field_assoc, [{atom, 0, type}, {bin, 0, [{bin_element, 0, {string, 0, "string"}, default, default}]}]},
                {type, 0, map_field_assoc, [{atom, 0, enum},
                    {type, 0, list, [
                        {bin, 0, [{bin_element, 0, {string, 0, ""}, default, default}]}
                    ]}
                ]}
            ]}
        ]}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(<<"string">>, maps:get(<<"type">>, Schema)),
    ?assertMatch(#{<<"not">> := _}, Schema),
    NotSchema = maps:get(<<"not">>, Schema),
    %% The negated schema should be an object with type and enum
    ?assertMatch(#{<<"type">> := _}, NotSchema).

%%%===================================================================
%%% Test: Integration - Using constraints in request types
%%%===================================================================

types_to_schemas_with_range_test() ->
    Types = [
        {age, {type, 1, range, [{integer, 1, 0}, {integer, 1, 150}]}},
        {user,
            {type, 1, map, [
                {type, 1, map_field_exact, [{atom, 1, name}, {type, 1, binary, []}]},
                {type, 1, map_field_exact, [{atom, 1, age}, {user_type, 1, age, []}]}
            ]}}
    ],
    Schemas = gm_type_schema_converter:types_to_schemas(Types),
    ?assertMatch(#{<<"Age">> := _}, Schemas),
    ?assertMatch(#{<<"User">> := _}, Schemas),
    AgeSchema = maps:get(<<"Age">>, Schemas),
    ?assertEqual(0, maps:get(<<"minimum">>, AgeSchema)),
    ?assertEqual(150, maps:get(<<"maximum">>, AgeSchema)).

