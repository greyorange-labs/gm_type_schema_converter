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

type_to_schema_union_atoms_enum_test() ->
    %% All-atom union should collapse to a single enum
    TypeDef = {status,
        {type, 1, union, [
            {atom, 1, active},
            {atom, 1, inactive}
        ]}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(<<"string">>, maps:get(<<"type">>, Schema)),
    ?assertEqual([<<"active">>, <<"inactive">>], maps:get(<<"enum">>, Schema)).

type_to_schema_union_mixed_test() ->
    %% Mixed union: atoms collapsed into one enum + other types
    TypeDef = {value,
        {type, 1, union, [
            {atom, 1, null},
            {atom, 1, undefined},
            {type, 1, binary, []}
        ]}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertMatch(#{<<"oneOf">> := _}, Schema),
    OneOf = maps:get(<<"oneOf">>, Schema),
    ?assertEqual(2, length(OneOf)),
    %% First should be the collapsed atom enum
    [AtomEnum | _] = OneOf,
    ?assertEqual(<<"string">>, maps:get(<<"type">>, AtomEnum)),
    ?assertEqual([<<"null">>, <<"undefined">>], maps:get(<<"enum">>, AtomEnum)).

type_to_schema_union_single_atom_mixed_test() ->
    %% Single atom in mixed union - no collapse (stays oneOf)
    TypeDef = {opt,
        {type, 1, union, [
            {atom, 1, none},
            {type, 1, integer, []}
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

type_to_schema_record_type_test() ->
    %% Record type reference should convert to object without warning
    %% e.g. -type movetaskrec() :: #movetaskrec{}.
    TypeDef = {movetaskrec, {type, {913, 24}, record, [{atom, {913, 25}, movetaskrec}]}},
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

%%%===================================================================
%%% Test: Phase 1 - Built-in Erlang Types
%%%===================================================================

non_neg_integer_test() ->
    TypeDef = {count, {type, 1, non_neg_integer, []}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(#{<<"type">> => <<"integer">>, <<"minimum">> => 0}, Schema).

pos_integer_test() ->
    TypeDef = {id, {type, 1, pos_integer, []}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(#{<<"type">> => <<"integer">>, <<"minimum">> => 1}, Schema).

neg_integer_test() ->
    TypeDef = {offset, {type, 1, neg_integer, []}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(#{<<"type">> => <<"integer">>, <<"maximum">> => -1}, Schema).

number_type_test() ->
    TypeDef = {val, {type, 1, number, []}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(#{<<"type">> => <<"number">>}, Schema).

nonempty_list_test() ->
    TypeDef = {items, {type, 1, nonempty_list, [{type, 1, binary, []}]}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(<<"array">>, maps:get(<<"type">>, Schema)),
    ?assertEqual(#{<<"type">> => <<"string">>}, maps:get(<<"items">>, Schema)),
    ?assertEqual(1, maps:get(<<"minItems">>, Schema)).

string_type_test() ->
    TypeDef = {name, {type, 1, string, []}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(#{<<"type">> => <<"string">>}, Schema).

byte_type_test() ->
    TypeDef = {b, {type, 1, byte, []}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(#{<<"type">> => <<"integer">>, <<"minimum">> => 0, <<"maximum">> => 255}, Schema).

char_type_test() ->
    TypeDef = {c, {type, 1, char, []}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(#{<<"type">> => <<"integer">>, <<"minimum">> => 0, <<"maximum">> => 1114111}, Schema).

arity_type_test() ->
    TypeDef = {a, {type, 1, arity, []}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(#{<<"type">> => <<"integer">>, <<"minimum">> => 0, <<"maximum">> => 255}, Schema).

atom_type_test() ->
    TypeDef = {a, {type, 1, atom, []}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(#{<<"type">> => <<"string">>}, Schema).

term_type_test() ->
    TypeDef = {t, {type, 1, term, []}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(#{}, Schema).

any_type_test() ->
    TypeDef = {a, {type, 1, any, []}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(#{}, Schema).

timeout_type_test() ->
    TypeDef = {t, {type, 1, timeout, []}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertMatch(#{<<"oneOf">> := _}, Schema),
    OneOf = maps:get(<<"oneOf">>, Schema),
    ?assertEqual(2, length(OneOf)),
    [IntSchema, InfSchema] = OneOf,
    ?assertEqual(<<"integer">>, maps:get(<<"type">>, IntSchema)),
    ?assertEqual(0, maps:get(<<"minimum">>, IntSchema)),
    ?assertEqual(<<"string">>, maps:get(<<"type">>, InfSchema)),
    ?assertEqual([<<"infinity">>], maps:get(<<"enum">>, InfSchema)).

generic_tuple_test() ->
    TypeDef = {t, {type, 1, tuple, any}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(#{<<"type">> => <<"array">>}, Schema).

specific_tuple_test() ->
    %% {binary(), integer()} -> array with fixed length 2
    TypeDef = {pair, {type, 1, tuple, [
        {type, 1, binary, []},
        {type, 1, integer, []}
    ]}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(<<"array">>, maps:get(<<"type">>, Schema)),
    ?assertEqual(2, maps:get(<<"minItems">>, Schema)),
    ?assertEqual(2, maps:get(<<"maxItems">>, Schema)),
    %% Items should be oneOf since types differ
    Items = maps:get(<<"items">>, Schema),
    ?assertMatch(#{<<"oneOf">> := _}, Items).

specific_tuple_same_types_test() ->
    %% {binary(), binary()} -> array with single items schema
    TypeDef = {pair, {type, 1, tuple, [
        {type, 1, binary, []},
        {type, 1, binary, []}
    ]}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(<<"array">>, maps:get(<<"type">>, Schema)),
    ?assertEqual(2, maps:get(<<"minItems">>, Schema)),
    ?assertEqual(2, maps:get(<<"maxItems">>, Schema)),
    %% Items should be single schema since types are the same
    ?assertEqual(#{<<"type">> => <<"string">>}, maps:get(<<"items">>, Schema)).

%%%===================================================================
%%% Test: Phase 2 - gm_type Format Types
%%%===================================================================

gm_type_date_test() ->
    TypeDef = {d, {remote_type, 1, [{atom, 1, gm_type}, {atom, 1, date}, []]}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(#{<<"type">> => <<"string">>, <<"format">> => <<"date">>}, Schema).

gm_type_datetime_test() ->
    TypeDef = {d, {remote_type, 1, [{atom, 1, gm_type}, {atom, 1, datetime}, []]}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(#{<<"type">> => <<"string">>, <<"format">> => <<"date-time">>}, Schema).

gm_type_email_test() ->
    TypeDef = {e, {remote_type, 1, [{atom, 1, gm_type}, {atom, 1, email}, []]}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(#{<<"type">> => <<"string">>, <<"format">> => <<"email">>}, Schema).

gm_type_uri_test() ->
    TypeDef = {u, {remote_type, 1, [{atom, 1, gm_type}, {atom, 1, uri}, []]}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(#{<<"type">> => <<"string">>, <<"format">> => <<"uri">>}, Schema).

gm_type_uuid_test() ->
    TypeDef = {u, {remote_type, 1, [{atom, 1, gm_type}, {atom, 1, uuid}, []]}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(#{<<"type">> => <<"string">>, <<"format">> => <<"uuid">>}, Schema).

gm_type_ipv4_test() ->
    TypeDef = {i, {remote_type, 1, [{atom, 1, gm_type}, {atom, 1, ipv4}, []]}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(#{<<"type">> => <<"string">>, <<"format">> => <<"ipv4">>}, Schema).

gm_type_ipv6_test() ->
    TypeDef = {i, {remote_type, 1, [{atom, 1, gm_type}, {atom, 1, ipv6}, []]}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(#{<<"type">> => <<"string">>, <<"format">> => <<"ipv6">>}, Schema).

gm_type_password_test() ->
    TypeDef = {p, {remote_type, 1, [{atom, 1, gm_type}, {atom, 1, password}, []]}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(#{<<"type">> => <<"string">>, <<"format">> => <<"password">>}, Schema).

gm_type_base64_test() ->
    TypeDef = {b, {remote_type, 1, [{atom, 1, gm_type}, {atom, 1, base64}, []]}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(#{<<"type">> => <<"string">>, <<"format">> => <<"byte">>}, Schema).

gm_type_hostname_test() ->
    TypeDef = {h, {remote_type, 1, [{atom, 1, gm_type}, {atom, 1, hostname}, []]}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(#{<<"type">> => <<"string">>, <<"format">> => <<"hostname">>}, Schema).

gm_type_int32_test() ->
    TypeDef = {i, {remote_type, 1, [{atom, 1, gm_type}, {atom, 1, int32}, []]}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(#{<<"type">> => <<"integer">>, <<"format">> => <<"int32">>}, Schema).

gm_type_int64_test() ->
    TypeDef = {i, {remote_type, 1, [{atom, 1, gm_type}, {atom, 1, int64}, []]}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(#{<<"type">> => <<"integer">>, <<"format">> => <<"int64">>}, Schema).

gm_type_double_test() ->
    TypeDef = {d, {remote_type, 1, [{atom, 1, gm_type}, {atom, 1, double}, []]}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(#{<<"type">> => <<"number">>, <<"format">> => <<"double">>}, Schema).

gm_type_nullable_test() ->
    %% gm_type:nullable(binary()) -> oneOf: [{type: string}, {type: null}]
    TypeDef = {n, {remote_type, 1, [{atom, 1, gm_type}, {atom, 1, nullable}, [{type, 1, binary, []}]]}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertMatch(#{<<"oneOf">> := _}, Schema),
    OneOf = maps:get(<<"oneOf">>, Schema),
    ?assertEqual(2, length(OneOf)),
    ?assertEqual(#{<<"type">> => <<"string">>}, lists:nth(1, OneOf)),
    ?assertEqual(#{<<"type">> => <<"null">>}, lists:nth(2, OneOf)).

gm_type_nullable_ref_test() ->
    %% gm_type:nullable(user_type) -> oneOf: [{$ref: ...}, {type: null}]
    TypeDef = {n, {remote_type, 1, [{atom, 1, gm_type}, {atom, 1, nullable}, [{user_type, 1, user_id, []}]]}},
    AllTypes = [{user_id, {type, 1, binary, []}}],
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, AllTypes),
    ?assertMatch(#{<<"oneOf">> := _}, Schema),
    OneOf = maps:get(<<"oneOf">>, Schema),
    ?assertEqual(2, length(OneOf)),
    ?assertMatch(#{<<"$ref">> := <<"#/components/schemas/UserId">>}, lists:nth(1, OneOf)),
    ?assertEqual(#{<<"type">> => <<"null">>}, lists:nth(2, OneOf)).

%%%===================================================================
%%% Test: Phase 3 - OpenAPI Metadata Features
%%%===================================================================

with_metadata_description_test() ->
    TypeDef = {user_id,
        {type, 1, tuple, [
            {atom, 1, with_metadata},
            {type, 1, map, [
                {type, 1, map_field_assoc, [{atom, 1, description}, {bin, 1, [{bin_element, 1, {string, 1, "Unique user identifier"}, default, default}]}]}
            ]},
            {type, 1, binary, []}
        ]}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(<<"string">>, maps:get(<<"type">>, Schema)),
    ?assertEqual(<<"Unique user identifier">>, maps:get(<<"description">>, Schema)).

with_metadata_multiple_keys_test() ->
    TypeDef = {user_id,
        {type, 1, tuple, [
            {atom, 1, with_metadata},
            {type, 1, map, [
                {type, 1, map_field_assoc, [{atom, 1, description}, {bin, 1, [{bin_element, 1, {string, 1, "User ID"}, default, default}]}]},
                {type, 1, map_field_assoc, [{atom, 1, example}, {bin, 1, [{bin_element, 1, {string, 1, "usr_abc123"}, default, default}]}]},
                {type, 1, map_field_assoc, [{atom, 1, deprecated}, {atom, 1, true}]}
            ]},
            {type, 1, binary, []}
        ]}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(<<"string">>, maps:get(<<"type">>, Schema)),
    ?assertEqual(<<"User ID">>, maps:get(<<"description">>, Schema)),
    ?assertEqual(<<"usr_abc123">>, maps:get(<<"example">>, Schema)),
    ?assertEqual(true, maps:get(<<"deprecated">>, Schema)).

with_metadata_title_test() ->
    TypeDef = {x,
        {type, 1, tuple, [
            {atom, 1, with_metadata},
            {type, 1, map, [
                {type, 1, map_field_assoc, [{atom, 1, title}, {bin, 1, [{bin_element, 1, {string, 1, "My Title"}, default, default}]}]}
            ]},
            {type, 1, integer, []}
        ]}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(<<"integer">>, maps:get(<<"type">>, Schema)),
    ?assertEqual(<<"My Title">>, maps:get(<<"title">>, Schema)).

with_metadata_read_write_only_test() ->
    TypeDef = {x,
        {type, 1, tuple, [
            {atom, 1, with_metadata},
            {type, 1, map, [
                {type, 1, map_field_assoc, [{atom, 1, read_only}, {atom, 1, true}]},
                {type, 1, map_field_assoc, [{atom, 1, description}, {bin, 1, [{bin_element, 1, {string, 1, "Read-only field"}, default, default}]}]}
            ]},
            {type, 1, binary, []}
        ]}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(true, maps:get(<<"readOnly">>, Schema)),
    ?assertEqual(<<"Read-only field">>, maps:get(<<"description">>, Schema)).

with_metadata_default_test() ->
    TypeDef = {x,
        {type, 1, tuple, [
            {atom, 1, with_metadata},
            {type, 1, map, [
                {type, 1, map_field_assoc, [{atom, 1, default}, {integer, 1, 42}]}
            ]},
            {type, 1, integer, []}
        ]}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(<<"integer">>, maps:get(<<"type">>, Schema)),
    ?assertEqual(42, maps:get(<<"default">>, Schema)).

additional_properties_false_test() ->
    TypeDef = {strict_config,
        {type, 1, tuple, [
            {atom, 1, map_with_constraints},
            {type, 1, map, [
                {type, 1, map_field_assoc, [{atom, 1, additional_properties}, {atom, 1, false}]}
            ]},
            {type, 1, map, [
                {type, 1, map_field_exact, [{atom, 1, name}, {type, 1, binary, []}]}
            ]}
        ]}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, []),
    ?assertEqual(<<"object">>, maps:get(<<"type">>, Schema)),
    ?assertEqual(false, maps:get(<<"additionalProperties">>, Schema)).

%%%===================================================================
%%% Test: Phase 4 - Advanced Features (Discriminated Unions)
%%%===================================================================

discriminated_union_test() ->
    AllTypes = [
        {credit_card, {type, 1, map, [
            {type, 1, map_field_exact, [{atom, 1, type}, {atom, 1, credit_card}]},
            {type, 1, map_field_exact, [{atom, 1, card_number}, {type, 1, binary, []}]}
        ]}},
        {paypal, {type, 1, map, [
            {type, 1, map_field_exact, [{atom, 1, type}, {atom, 1, paypal}]},
            {type, 1, map_field_exact, [{atom, 1, email}, {type, 1, binary, []}]}
        ]}}
    ],
    TypeDef = {payment,
        {type, 1, tuple, [
            {atom, 1, discriminated_union},
            {atom, 1, type},
            {cons, 1, {user_type, 1, credit_card, []},
                {cons, 1, {user_type, 1, paypal, []},
                    {nil, 1}}}
        ]}},
    Schema = gm_type_schema_converter:type_to_schema(TypeDef, AllTypes),
    ?assertMatch(#{<<"oneOf">> := _, <<"discriminator">> := _}, Schema),
    OneOf = maps:get(<<"oneOf">>, Schema),
    ?assertEqual(2, length(OneOf)),
    ?assertMatch(#{<<"$ref">> := <<"#/components/schemas/CreditCard">>}, lists:nth(1, OneOf)),
    ?assertMatch(#{<<"$ref">> := <<"#/components/schemas/Paypal">>}, lists:nth(2, OneOf)),
    Discriminator = maps:get(<<"discriminator">>, Schema),
    ?assertEqual(#{<<"propertyName">> => <<"type">>}, Discriminator).

