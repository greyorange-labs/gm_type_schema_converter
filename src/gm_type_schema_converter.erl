%%%-------------------------------------------------------------------
%%% @author amarBitMan <https://github.com/amarBitMan>
%%% @copyright (C) 2025, Grey Orange
%%%-------------------------------------------------------------------
-module(gm_type_schema_converter).

-moduledoc """
----------------------------------------------------------------------
Type to JSON Schema Converter

Converts Erlang type definitions (Abstract Syntax Trees) to
JSON Schema format (compatible with OpenAPI 3.0.x).

This library is shared between:
- rebar3_openapi plugin (for documentation generation)
- Runtime schema validation (for request/response validation)
----------------------------------------------------------------------
""".

-export([
    type_to_schema/2,
    types_to_schemas/1,
    capitalize_type_name/1
]).

-export_type([
    type_def/0,
    json_schema/0
]).

%%%===================================================================
%%% Types
%%%===================================================================

-type type_def() :: {atom(), erl_parse:abstract_type()}.
-type json_schema() :: map().

%%%===================================================================
%%% Public API
%%%===================================================================

-doc """
----------------------------------------------------------------------
Convert a single type definition to JSON Schema.
Takes a {TypeName, TypeAST} tuple and a list of all type definitions
for reference resolution. Returns a JSON Schema map.
----------------------------------------------------------------------
""".
-spec type_to_schema(
    TypeDef :: type_def(),
    AllTypes :: [type_def()]
) -> json_schema().
type_to_schema({_TypeName, TypeDefAST}, AllTypes) ->
    convert_type_definition(TypeDefAST, AllTypes, []).

-doc """
----------------------------------------------------------------------
Convert all type definitions to a JSON Schemas map.
Returns a map of capitalized type names to JSON Schema maps.
----------------------------------------------------------------------
""".
-spec types_to_schemas([type_def()]) -> #{binary() => json_schema()}.
types_to_schemas(Types) ->
    lists:foldl(
        fun({TypeName, _TypeDef} = TypeTuple, Acc) ->
            Schema = type_to_schema(TypeTuple, Types),
            SchemaName = capitalize_type_name(TypeName),
            Acc#{SchemaName => Schema}
        end,
        #{},
        Types
    ).

-doc """
----------------------------------------------------------------------
Capitalize type name for schema naming convention.
Examples: user_profile -> <<"UserProfile">>, user -> <<"User">>
----------------------------------------------------------------------
""".
-spec capitalize_type_name(atom()) -> binary().
capitalize_type_name(TypeName) ->
    TypeStr = atom_to_list(TypeName),
    Words = string:split(TypeStr, "_", all),
    CapitalizedWords = lists:map(fun capitalize_word/1, Words),
    list_to_binary(string:join(CapitalizedWords, "")).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-doc false.
-spec convert_type_to_schema(
    {atom(), erl_parse:abstract_type()} | undefined,
    [type_def()],
    [atom()]
) -> json_schema().
convert_type_to_schema(undefined, _AllTypes, _Visited) ->
    #{<<"type">> => <<"object">>};
convert_type_to_schema({TypeName, TypeDef}, AllTypes, Visited) ->
    case lists:member(TypeName, Visited) of
        true ->
            SchemaName = capitalize_type_name(TypeName),
            #{<<"$ref">> => <<"#/components/schemas/", SchemaName/binary>>};
        false ->
            convert_type_definition(TypeDef, AllTypes, [TypeName | Visited])
    end.

-doc false.
-spec convert_type_definition(
    erl_parse:abstract_type(),
    [type_def()],
    [atom()]
) -> json_schema().

%% Union type -> oneOf (with atom-enum optimization)
convert_type_definition({type, _Line, union, Types}, AllTypes, Visited) ->
    case partition_atoms_and_types(Types) of
        {Atoms, []} when length(Atoms) > 0 ->
            EnumValues = [atom_to_binary(A, utf8) || A <- Atoms],
            #{<<"type">> => <<"string">>, <<"enum">> => EnumValues};
        {Atoms, NonAtoms} when length(Atoms) > 1 ->
            AtomEnum = #{<<"type">> => <<"string">>, <<"enum">> => [atom_to_binary(A, utf8) || A <- Atoms]},
            OtherSchemas = lists:map(
                fun(Type) -> convert_type_definition(Type, AllTypes, Visited) end,
                NonAtoms
            ),
            #{<<"oneOf">> => [AtomEnum | OtherSchemas]};
        _ ->
            Schemas = lists:map(
                fun(Type) -> convert_type_definition(Type, AllTypes, Visited) end,
                Types
            ),
            #{<<"oneOf">> => Schemas}
    end;
%% List type -> array
convert_type_definition({type, _Line, list, [ItemType]}, AllTypes, Visited) ->
    ItemsSchema = convert_type_definition(ItemType, AllTypes, Visited),
    #{
        <<"type">> => <<"array">>,
        <<"items">> => ItemsSchema
    };
%% Map with field definitions -> object with properties
convert_type_definition({type, _Line, map, Fields}, AllTypes, Visited) when is_list(Fields) ->
    case is_map_with_fields(Fields) of
        true ->
            convert_map_with_fields(Fields, AllTypes, Visited);
        false ->
            convert_generic_map(Fields, AllTypes, Visited)
    end;
%% Fallback for empty map
convert_type_definition({type, _Line, map, any}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"object">>};
%% Integer range: 0..150 (native Erlang syntax)
convert_type_definition({type, _Line, range, [{integer, _, Min}, {integer, _, Max}]}, _AllTypes, _Visited) ->
    #{
        <<"type">> => <<"integer">>,
        <<"minimum">> => Min,
        <<"maximum">> => Max
    };
%% Primitive types
convert_type_definition({type, _Line, binary, []}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"string">>};
convert_type_definition({type, _Line, integer, []}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"integer">>};
convert_type_definition({type, _Line, float, []}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"number">>};
convert_type_definition({type, _Line, boolean, []}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"boolean">>};
%% Extended integer types
convert_type_definition({type, _Line, non_neg_integer, []}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"integer">>, <<"minimum">> => 0};
convert_type_definition({type, _Line, pos_integer, []}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"integer">>, <<"minimum">> => 1};
convert_type_definition({type, _Line, neg_integer, []}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"integer">>, <<"maximum">> => -1};
%% number() -> {type: number}
convert_type_definition({type, _Line, number, []}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"number">>};
%% nonempty_list(T) -> {type: array, items: T, minItems: 1}
convert_type_definition({type, _Line, nonempty_list, [ItemType]}, AllTypes, Visited) ->
    ItemsSchema = convert_type_definition(ItemType, AllTypes, Visited),
    #{
        <<"type">> => <<"array">>,
        <<"items">> => ItemsSchema,
        <<"minItems">> => 1
    };
%% string() -> {type: string}
convert_type_definition({type, _Line, string, []}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"string">>};
%% byte() -> {type: integer, minimum: 0, maximum: 255}
convert_type_definition({type, _Line, byte, []}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"integer">>, <<"minimum">> => 0, <<"maximum">> => 255};
%% char() -> {type: integer, minimum: 0, maximum: 1114111}
convert_type_definition({type, _Line, char, []}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"integer">>, <<"minimum">> => 0, <<"maximum">> => 1114111};
%% arity() -> {type: integer, minimum: 0, maximum: 255}
convert_type_definition({type, _Line, arity, []}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"integer">>, <<"minimum">> => 0, <<"maximum">> => 255};
%% atom() -> {type: string}
convert_type_definition({type, _Line, atom, []}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"string">>};
%% term() / any() -> {} (empty schema = accepts any value)
convert_type_definition({type, _Line, term, []}, _AllTypes, _Visited) ->
    #{};
convert_type_definition({type, _Line, any, []}, _AllTypes, _Visited) ->
    #{};
%% timeout() -> oneOf: [non_neg_integer, 'infinity']
convert_type_definition({type, _Line, timeout, []}, _AllTypes, _Visited) ->
    #{
        <<"oneOf">> => [
            #{<<"type">> => <<"integer">>, <<"minimum">> => 0},
            #{<<"type">> => <<"string">>, <<"enum">> => [<<"infinity">>]}
        ]
    };
%% Generic tuple() -> {type: array}
convert_type_definition({type, _Line, tuple, any}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"array">>};
%% Atom literal -> string enum
convert_type_definition({atom, _Line, Atom}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"string">>, <<"enum">> => [atom_to_binary(Atom, utf8)]};
%% Integer literal -> integer const (e.g. code => 400 in a map type)
convert_type_definition({integer, _Line, Value}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"integer">>, <<"enum">> => [Value]};
%% gm_type remote types -> string with format
convert_type_definition({remote_type, _Line, [{atom, _, gm_type}, {atom, _, date}, []]}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"string">>, <<"format">> => <<"date">>};
convert_type_definition({remote_type, _Line, [{atom, _, gm_type}, {atom, _, datetime}, []]}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"string">>, <<"format">> => <<"date-time">>};
convert_type_definition({remote_type, _Line, [{atom, _, gm_type}, {atom, _, email}, []]}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"string">>, <<"format">> => <<"email">>};
convert_type_definition({remote_type, _Line, [{atom, _, gm_type}, {atom, _, uri}, []]}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"string">>, <<"format">> => <<"uri">>};
convert_type_definition({remote_type, _Line, [{atom, _, gm_type}, {atom, _, uuid}, []]}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"string">>, <<"format">> => <<"uuid">>};
convert_type_definition({remote_type, _Line, [{atom, _, gm_type}, {atom, _, ipv4}, []]}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"string">>, <<"format">> => <<"ipv4">>};
convert_type_definition({remote_type, _Line, [{atom, _, gm_type}, {atom, _, ipv6}, []]}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"string">>, <<"format">> => <<"ipv6">>};
convert_type_definition({remote_type, _Line, [{atom, _, gm_type}, {atom, _, password}, []]}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"string">>, <<"format">> => <<"password">>};
convert_type_definition({remote_type, _Line, [{atom, _, gm_type}, {atom, _, base64}, []]}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"string">>, <<"format">> => <<"byte">>};
convert_type_definition({remote_type, _Line, [{atom, _, gm_type}, {atom, _, hostname}, []]}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"string">>, <<"format">> => <<"hostname">>};
%% gm_type remote types -> number with format
convert_type_definition({remote_type, _Line, [{atom, _, gm_type}, {atom, _, int32}, []]}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"integer">>, <<"format">> => <<"int32">>};
convert_type_definition({remote_type, _Line, [{atom, _, gm_type}, {atom, _, int64}, []]}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"integer">>, <<"format">> => <<"int64">>};
convert_type_definition({remote_type, _Line, [{atom, _, gm_type}, {atom, _, double}, []]}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"number">>, <<"format">> => <<"double">>};
%% gm_type:nullable(T) -> oneOf: [T, {type: null}]
convert_type_definition({remote_type, _Line, [{atom, _, gm_type}, {atom, _, nullable}, [InnerType]]}, AllTypes, Visited) ->
    InnerSchema = convert_type_definition(InnerType, AllTypes, Visited),
    #{<<"oneOf">> => [InnerSchema, #{<<"type">> => <<"null">>}]};
%% Remote type reference (e.g., module:type())
convert_type_definition({remote_type, _Line, [{atom, _, _Module}, {atom, _, TypeName}, []]}, AllTypes, Visited) ->
    case find_type_definition(TypeName, AllTypes) of
        {TypeName, _TypeDef} = Found ->
            convert_type_to_schema(Found, AllTypes, Visited);
        undefined ->
            #{<<"type">> => <<"object">>}
    end;
%% User-defined type reference
convert_type_definition({user_type, _Line, TypeName, []}, AllTypes, _Visited) ->
    case find_type_definition(TypeName, AllTypes) of
        {TypeName, _TypeDef} ->
            SchemaName = capitalize_type_name(TypeName),
            #{<<"$ref">> => <<"#/components/schemas/", SchemaName/binary>>};
        undefined ->
            SchemaName = capitalize_type_name(TypeName),
            #{<<"$ref">> => <<"#/components/schemas/", SchemaName/binary>>}
    end;
%% Metadata wrapper: {with_metadata, MetadataMap, InnerType}
convert_type_definition(
    {type, _Line, tuple, [
        {atom, _, with_metadata},
        MetadataMapAST,
        InnerType
    ]},
    AllTypes,
    Visited
) ->
    InnerSchema = convert_type_definition(InnerType, AllTypes, Visited),
    Metadata = extract_constraint_map(MetadataMapAST, AllTypes, Visited),
    add_metadata(InnerSchema, Metadata);
%% anyOf required constraint: {any_of_required, [Key1, Key2, ...], BaseMapType}
convert_type_definition(
    {type, _Line, tuple, [
        {atom, _, any_of_required},
        KeyListAST,
        BaseMapType
    ]},
    AllTypes,
    Visited
) ->
    RequiredKeys = extract_keys_from_list(KeyListAST),
    BaseSchema = convert_type_definition(BaseMapType, AllTypes, Visited),
    AnyOfSchemas = generate_any_of_required_schemas(RequiredKeys),
    BaseSchema#{<<"anyOf">> => AnyOfSchemas};
%% Float constraints wrapper: {float_with_constraints, ConstraintsMap}
convert_type_definition(
    {type, _Line, tuple, [
        {atom, _, float_with_constraints},
        ConstraintsMapAST
    ]},
    AllTypes,
    Visited
) ->
    BaseSchema = #{<<"type">> => <<"number">>},
    Constraints = extract_constraint_map(ConstraintsMapAST, AllTypes, Visited),
    add_numeric_constraints(BaseSchema, Constraints);
%% String constraints wrapper: {binary_with_constraints, ConstraintsMap}
convert_type_definition(
    {type, _Line, tuple, [
        {atom, _, binary_with_constraints},
        ConstraintsMapAST
    ]},
    AllTypes,
    Visited
) ->
    BaseSchema = #{<<"type">> => <<"string">>},
    Constraints = extract_constraint_map(ConstraintsMapAST, AllTypes, Visited),
    add_string_constraints(BaseSchema, Constraints);
%% Array constraints wrapper: {list_with_constraints, ItemType, ConstraintsMap}
convert_type_definition(
    {type, _Line, tuple, [
        {atom, _, list_with_constraints},
        ItemType,
        ConstraintsMapAST
    ]},
    AllTypes,
    Visited
) ->
    ItemsSchema = convert_type_definition(ItemType, AllTypes, Visited),
    BaseSchema = #{
        <<"type">> => <<"array">>,
        <<"items">> => ItemsSchema
    },
    Constraints = extract_constraint_map(ConstraintsMapAST, AllTypes, Visited),
    add_array_constraints(BaseSchema, Constraints);
%% Object constraints wrapper: {map_with_constraints, ConstraintsMap, BaseMapType}
convert_type_definition(
    {type, _Line, tuple, [
        {atom, _, map_with_constraints},
        ConstraintsMapAST,
        BaseMapType
    ]},
    AllTypes,
    Visited
) ->
    BaseSchema = convert_type_definition(BaseMapType, AllTypes, Visited),
    Constraints = extract_constraint_map(ConstraintsMapAST, AllTypes, Visited),
    add_object_constraints(BaseSchema, Constraints);
%% allOf required constraint: {all_of_required, {Key1, Key2, ...}, BaseMapType}
convert_type_definition(
    {type, _Line, tuple, [
        {atom, _, all_of_required},
        KeyTuple,
        BaseMapType
    ]},
    AllTypes,
    Visited
) ->
    RequiredKeys = extract_keys_from_list(KeyTuple),
    BaseSchema = convert_type_definition(BaseMapType, AllTypes, Visited),
    AllOfSchemas = generate_all_of_required_schemas(RequiredKeys),
    BaseSchema#{<<"allOf">> => AllOfSchemas};
%% not constraint: {not_constraint, BaseType, NegatedSchemaAST}
convert_type_definition(
    {type, _Line, tuple, [
        {atom, _, not_constraint},
        BaseType,
        NegatedSchemaAST
    ]},
    AllTypes,
    Visited
) ->
    BaseSchema = convert_type_definition(BaseType, AllTypes, Visited),
    NegatedSchema = convert_type_definition(NegatedSchemaAST, AllTypes, Visited),
    BaseSchema#{<<"not">> => NegatedSchema};
%% Discriminated union: {discriminated_union, PropertyName, [Type1, Type2, ...]}
convert_type_definition(
    {type, _Line, tuple, [
        {atom, _, discriminated_union},
        {atom, _, PropertyName},
        VariantsList
    ]},
    _AllTypes,
    _Visited
) ->
    Variants = extract_variant_types(VariantsList),
    OneOfSchemas = lists:map(
        fun(TypeName) ->
            SchemaName = capitalize_type_name(TypeName),
            #{<<"$ref">> => <<"#/components/schemas/", SchemaName/binary>>}
        end,
        Variants
    ),
    #{
        <<"oneOf">> => OneOfSchemas,
        <<"discriminator">> => #{<<"propertyName">> => atom_to_binary(PropertyName, utf8)}
    };
%% Specific tuple types (non-wrapper) -> array with fixed length
%% Must come AFTER all wrapper tuple clauses above
convert_type_definition({type, _Line, tuple, Elements}, AllTypes, Visited) when is_list(Elements) ->
    N = length(Elements),
    ElementSchemas = lists:map(
        fun(E) -> convert_type_definition(E, AllTypes, Visited) end,
        Elements
    ),
    UniqueSchemas = lists:usort(ElementSchemas),
    ItemsSchema =
        case UniqueSchemas of
            [Single] -> Single;
            Multiple -> #{<<"oneOf">> => Multiple}
        end,
    #{
        <<"type">> => <<"array">>,
        <<"items">> => ItemsSchema,
        <<"minItems">> => N,
        <<"maxItems">> => N
    };
%% Record type reference -> object
%% Records in type specs appear as {type, Line, record, [{atom, L, RecordName} | Fields]}
%% Record field definitions aren't available in the type AST, so convert to object
convert_type_definition({type, _Line, record, [{atom, _, _RecordName} | _]}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"object">>};
%% nil type (empty list []) -> array with maxItems 0
convert_type_definition({type, _Line, nil, []}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"array">>, <<"maxItems">> => 0};
%% Bare list() without type parameter -> array
convert_type_definition({type, _Line, list, []}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"array">>};
%% Annotated type (Name :: Type) -> convert the underlying type, ignore the variable name
convert_type_definition({ann_type, _Line, [_Var, Type]}, AllTypes, Visited) ->
    convert_type_definition(Type, AllTypes, Visited);
%% Character literal ($a) -> integer const (characters are integers in Erlang)
convert_type_definition({char, _Line, Value}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"integer">>, <<"enum">> => [Value]};
%% Unary operator on type (e.g., -1 in a type spec)
convert_type_definition({op, _Line, '-', {integer, _, Value}}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"integer">>, <<"enum">> => [-Value]};
convert_type_definition({op, _Line, '+', {integer, _, Value}}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"integer">>, <<"enum">> => [Value]};
%% Sized binary types: <<_:N>> or <<_:_*N>> or <<_:N, _:_*M>>
convert_type_definition({type, _Line, binary, [_Size, _Unit]}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"string">>};
%% bitstring() -> string
convert_type_definition({type, _Line, bitstring, []}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"string">>};
%% nonempty_string() -> string with minLength 1
convert_type_definition({type, _Line, nonempty_string, []}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"string">>, <<"minLength">> => 1};
%% List variant types -> array
convert_type_definition({type, _Line, maybe_improper_list, []}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"array">>};
convert_type_definition({type, _Line, maybe_improper_list, [ItemType, _TailType]}, AllTypes, Visited) ->
    ItemsSchema = convert_type_definition(ItemType, AllTypes, Visited),
    #{<<"type">> => <<"array">>, <<"items">> => ItemsSchema};
convert_type_definition({type, _Line, nonempty_improper_list, [ItemType, _TailType]}, AllTypes, Visited) ->
    ItemsSchema = convert_type_definition(ItemType, AllTypes, Visited),
    #{<<"type">> => <<"array">>, <<"items">> => ItemsSchema, <<"minItems">> => 1};
convert_type_definition({type, _Line, nonempty_maybe_improper_list, []}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"array">>, <<"minItems">> => 1};
convert_type_definition({type, _Line, nonempty_maybe_improper_list, [ItemType, _TailType]}, AllTypes, Visited) ->
    ItemsSchema = convert_type_definition(ItemType, AllTypes, Visited),
    #{<<"type">> => <<"array">>, <<"items">> => ItemsSchema, <<"minItems">> => 1};
%% Process/system types -> string
convert_type_definition({type, _Line, pid, []}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"string">>};
convert_type_definition({type, _Line, port, []}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"string">>};
convert_type_definition({type, _Line, reference, []}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"string">>};
convert_type_definition({type, _Line, node, []}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"string">>};
convert_type_definition({type, _Line, module, []}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"string">>};
%% identifier() -> string (pid | port | reference)
convert_type_definition({type, _Line, identifier, []}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"string">>};
%% iodata() / iolist() -> string (binary data in JSON)
convert_type_definition({type, _Line, iodata, []}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"string">>};
convert_type_definition({type, _Line, iolist, []}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"string">>};
%% mfa() -> array (3-tuple: {module, function, arity})
convert_type_definition({type, _Line, mfa, []}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"array">>, <<"minItems">> => 3, <<"maxItems">> => 3};
%% none() / no_return() -> impossible type (nothing validates)
convert_type_definition({type, _Line, none, []}, _AllTypes, _Visited) ->
    #{<<"not">> => #{}};
convert_type_definition({type, _Line, no_return, []}, _AllTypes, _Visited) ->
    #{<<"not">> => #{}};
%% fun() types -> empty schema (functions cannot be serialized to JSON)
convert_type_definition({type, _Line, 'fun', _Args}, _AllTypes, _Visited) ->
    #{};
convert_type_definition({type, _Line, bounded_fun, _Args}, _AllTypes, _Visited) ->
    #{};
%% product type -> array
convert_type_definition({type, _Line, product, Elements}, AllTypes, Visited) ->
    Schemas = lists:map(fun(E) -> convert_type_definition(E, AllTypes, Visited) end, Elements),
    #{<<"type">> => <<"array">>, <<"items">> => #{<<"oneOf">> => Schemas}};
%% Type variable -> empty schema (accepts any, like a generic parameter)
convert_type_definition({var, _Line, _VarName}, _AllTypes, _Visited) ->
    #{};
%% Binary type spec in type position (e.g., <<>> or <<"string">>)
convert_type_definition({bin, _Line, _Elements}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"string">>};
%% Fallback for unknown types
convert_type_definition(Other, _AllTypes, _Visited) ->
    logger:warning("gm_type_schema_converter: unknown type AST, falling back to object: ~p", [Other]),
    #{<<"type">> => <<"object">>}.

%%%===================================================================
%%% Internal Helpers
%%%===================================================================

-doc false.
-spec extract_variant_types(erl_parse:abstract_form()) -> [atom()].
extract_variant_types({cons, _Line, {user_type, _, TypeName, []}, Rest}) ->
    [TypeName | extract_variant_types(Rest)];
extract_variant_types({nil, _Line}) ->
    [];
extract_variant_types({type, _Line, list, [{type, _, union, Types}]}) ->
    lists:map(
        fun
            ({user_type, _, TypeName, []}) -> TypeName;
            (Other) -> error({invalid_variant_type, Other})
        end,
        Types
    );
extract_variant_types(Other) ->
    error({invalid_variant_list, Other}).

-doc false.
-spec partition_atoms_and_types([erl_parse:abstract_type()]) -> {[atom()], [erl_parse:abstract_type()]}.
partition_atoms_and_types(Types) ->
    lists:foldl(
        fun
            ({atom, _, Atom}, {Atoms, Others}) ->
                {Atoms ++ [Atom], Others};
            (Other, {Atoms, Others}) ->
                {Atoms, Others ++ [Other]}
        end,
        {[], []},
        Types
    ).

-doc false.
-spec is_map_with_fields(list()) -> boolean().
is_map_with_fields([]) ->
    false;
is_map_with_fields([{type, _, map_field_exact, _} | _]) ->
    true;
is_map_with_fields([{type, _, map_field_assoc, _} | _]) ->
    true;
is_map_with_fields(_) ->
    false.

-doc false.
-spec convert_map_with_fields(
    list(),
    [type_def()],
    [atom()]
) -> json_schema().
convert_map_with_fields(Fields, AllTypes, Visited) ->
    {Properties, Required} = lists:foldl(
        fun(Field, {PropsAcc, ReqAcc}) ->
            case convert_map_field(Field, AllTypes, Visited) of
                {Key, Schema, IsRequired} when is_binary(Key) ->
                    NewReqAcc =
                        case IsRequired of
                            true -> [Key | ReqAcc];
                            false -> ReqAcc
                        end,
                    {PropsAcc#{Key => Schema}, NewReqAcc};
                {error, _Reason} ->
                    {PropsAcc, ReqAcc}
            end
        end,
        {#{}, []},
        Fields
    ),
    Result = #{
        <<"type">> => <<"object">>,
        <<"properties">> => Properties
    },
    case Required of
        [] ->
            Result;
        _ ->
            Result#{<<"required">> => lists:reverse(Required)}
    end.

-doc false.
-spec convert_generic_map(
    list(),
    [type_def()],
    [atom()]
) -> json_schema().
convert_generic_map([_KeyType, ValueType], AllTypes, Visited) ->
    ValueSchema = convert_type_definition(ValueType, AllTypes, Visited),
    #{
        <<"type">> => <<"object">>,
        <<"additionalProperties">> => ValueSchema
    };
convert_generic_map(_, _AllTypes, _Visited) ->
    #{<<"type">> => <<"object">>}.

-doc false.
-spec convert_map_field(
    erl_parse:abstract_type(),
    [type_def()],
    [atom()]
) -> {binary(), json_schema(), boolean()} | {error, term()}.
convert_map_field({type, _Line, map_field_exact, [{atom, _, Key}, ValueType]}, AllTypes, Visited) ->
    ValueSchema = convert_type_definition(ValueType, AllTypes, Visited),
    {atom_to_binary(Key, utf8), ValueSchema, true};
convert_map_field({type, _Line, map_field_assoc, [{atom, _, Key}, ValueType]}, AllTypes, Visited) ->
    ValueSchema = convert_type_definition(ValueType, AllTypes, Visited),
    {atom_to_binary(Key, utf8), ValueSchema, false};
convert_map_field(_Other, _AllTypes, _Visited) ->
    {error, invalid_field}.

-doc false.
-spec find_type_definition(atom(), [type_def()]) ->
    type_def() | undefined.
find_type_definition(TypeName, AllTypes) ->
    case lists:keyfind(TypeName, 1, AllTypes) of
        false -> undefined;
        Found -> Found
    end.

-doc false.
-spec capitalize_word(string()) -> string().
capitalize_word([]) ->
    [];
capitalize_word([First | Rest]) ->
    [string:to_upper(First) | Rest].

-doc false.
-spec extract_keys_from_list(erl_parse:abstract_form()) -> [atom()].
extract_keys_from_list({cons, _Line, {atom, _, Key}, Rest}) ->
    [Key | extract_keys_from_list(Rest)];
extract_keys_from_list({nil, _Line}) ->
    [];
extract_keys_from_list({tuple, _Line, Elements}) ->
    lists:map(
        fun
            ({atom, _, Key}) -> Key;
            (Other) -> error({invalid_key_in_tuple, Other})
        end,
        Elements
    );
extract_keys_from_list({type, _Line, tuple, Elements}) ->
    lists:map(
        fun
            ({atom, _, Key}) -> Key;
            (Other) -> error({invalid_key_in_tuple, Other})
        end,
        Elements
    );
extract_keys_from_list({type, _Line, list, [{type, _Line2, union, Types}]}) ->
    lists:map(
        fun
            ({user_type, _, TypeName, []}) -> TypeName;
            ({atom, _, Atom}) -> Atom;
            (Other) -> error({invalid_union_type, Other})
        end,
        Types
    );
extract_keys_from_list(Other) ->
    error({invalid_key_list, Other}).

-doc false.
-spec generate_any_of_required_schemas([atom()]) -> [json_schema()].
generate_any_of_required_schemas(Keys) ->
    lists:map(
        fun(Key) ->
            KeyBinary = atom_to_binary(Key, utf8),
            #{<<"required">> => [KeyBinary]}
        end,
        Keys
    ).

-doc false.
-spec generate_all_of_required_schemas([atom()]) -> [json_schema()].
generate_all_of_required_schemas(Keys) ->
    lists:map(
        fun(Key) ->
            KeyBinary = atom_to_binary(Key, utf8),
            #{<<"required">> => [KeyBinary]}
        end,
        Keys
    ).

-doc false.
-spec extract_constraint_map(erl_parse:abstract_type(), [type_def()], [atom()]) -> map().
extract_constraint_map({type, _Line, map, Fields}, _AllTypes, _Visited) ->
    lists:foldl(
        fun(Field, Acc) ->
            case Field of
                {type, _, map_field_assoc, [{atom, _, Key}, ValueAST]} ->
                    Value = extract_constraint_value_from_ast(ValueAST),
                    Acc#{Key => Value};
                {type, _, map_field_exact, [{atom, _, Key}, ValueAST]} ->
                    Value = extract_constraint_value_from_ast(ValueAST),
                    Acc#{Key => Value};
                _ ->
                    Acc
            end
        end,
        #{},
        Fields
    );
extract_constraint_map(_Other, _AllTypes, _Visited) ->
    #{}.

-doc false.
-spec extract_constraint_value_from_ast(erl_parse:abstract_type()) -> term().
extract_constraint_value_from_ast({integer, _, Value}) ->
    Value;
extract_constraint_value_from_ast({float, _, Value}) ->
    Value;
extract_constraint_value_from_ast({atom, _, Value}) ->
    Value;
extract_constraint_value_from_ast({bin, _, Elements}) ->
    extract_binary_value(Elements);
extract_constraint_value_from_ast({string, _, Value}) ->
    list_to_binary(Value);
extract_constraint_value_from_ast(Other) ->
    Other.

-doc false.
-spec extract_binary_value([term()]) -> binary().
extract_binary_value(Elements) ->
    lists:foldl(
        fun(Element, Acc) ->
            case Element of
                {bin_element, _, {string, _, String}, _, _} ->
                    <<Acc/binary, (list_to_binary(String))/binary>>;
                {bin_element, _, {integer, _, Int}, _, _} ->
                    <<Acc/binary, Int:8>>;
                {bin_element, _, Value, _, _} when is_binary(Value) ->
                    <<Acc/binary, Value/binary>>;
                _ ->
                    Acc
            end
        end,
        <<>>,
        Elements
    ).

-doc false.
-spec add_numeric_constraints(json_schema(), map()) -> json_schema().
add_numeric_constraints(Schema, Constraints) ->
    lists:foldl(
        fun({Key, Value}, Acc) ->
            case Key of
                minimum -> Acc#{<<"minimum">> => Value};
                maximum -> Acc#{<<"maximum">> => Value};
                exclusive_minimum -> Acc#{<<"exclusiveMinimum">> => Value};
                exclusive_maximum -> Acc#{<<"exclusiveMaximum">> => Value};
                multiple_of -> Acc#{<<"multipleOf">> => Value};
                _ -> Acc
            end
        end,
        Schema,
        maps:to_list(Constraints)
    ).

-doc false.
-spec add_string_constraints(json_schema(), map()) -> json_schema().
add_string_constraints(Schema, Constraints) ->
    lists:foldl(
        fun({Key, Value}, Acc) ->
            case Key of
                min_length ->
                    Acc#{<<"minLength">> => Value};
                max_length ->
                    Acc#{<<"maxLength">> => Value};
                pattern ->
                    Pattern =
                        case is_binary(Value) of
                            true -> binary_to_list(Value);
                            false -> Value
                        end,
                    Acc#{<<"pattern">> => list_to_binary(Pattern)};
                format ->
                    Format =
                        case is_binary(Value) of
                            true -> binary_to_list(Value);
                            false -> Value
                        end,
                    Acc#{<<"format">> => list_to_binary(Format)};
                _ ->
                    Acc
            end
        end,
        Schema,
        maps:to_list(Constraints)
    ).

-doc false.
-spec add_array_constraints(json_schema(), map()) -> json_schema().
add_array_constraints(Schema, Constraints) ->
    lists:foldl(
        fun({Key, Value}, Acc) ->
            case Key of
                min_items ->
                    Acc#{<<"minItems">> => Value};
                max_items ->
                    Acc#{<<"maxItems">> => Value};
                unique_items ->
                    Acc#{<<"uniqueItems">> => Value};
                contains ->
                    ContainsSchema = convert_type_definition(Value, [], []),
                    Acc#{<<"contains">> => ContainsSchema};
                _ ->
                    Acc
            end
        end,
        Schema,
        maps:to_list(Constraints)
    ).

-doc false.
-spec add_object_constraints(json_schema(), map()) -> json_schema().
add_object_constraints(Schema, Constraints) ->
    lists:foldl(
        fun({Key, Value}, Acc) ->
            case Key of
                min_properties ->
                    Acc#{<<"minProperties">> => Value};
                max_properties ->
                    Acc#{<<"maxProperties">> => Value};
                property_names ->
                    PropNamesSchema = convert_type_definition(Value, [], []),
                    Acc#{<<"propertyNames">> => PropNamesSchema};
                pattern_properties ->
                    PatternProps = convert_pattern_properties(Value),
                    Acc#{<<"patternProperties">> => PatternProps};
                dependencies ->
                    Deps = convert_dependencies(Value),
                    Acc#{<<"dependencies">> => Deps};
                additional_properties when is_boolean(Value) ->
                    Acc#{<<"additionalProperties">> => Value};
                _ ->
                    Acc
            end
        end,
        Schema,
        maps:to_list(Constraints)
    ).

-doc false.
-spec add_metadata(json_schema(), map()) -> json_schema().
add_metadata(Schema, Metadata) ->
    lists:foldl(
        fun({Key, Value}, Acc) ->
            case Key of
                description ->
                    Acc#{<<"description">> => ensure_binary(Value)};
                title ->
                    Acc#{<<"title">> => ensure_binary(Value)};
                default ->
                    Acc#{<<"default">> => Value};
                example ->
                    Acc#{<<"example">> => Value};
                deprecated when is_boolean(Value) ->
                    Acc#{<<"deprecated">> => Value};
                read_only when is_boolean(Value) ->
                    Acc#{<<"readOnly">> => Value};
                write_only when is_boolean(Value) ->
                    Acc#{<<"writeOnly">> => Value};
                _ ->
                    Acc
            end
        end,
        Schema,
        maps:to_list(Metadata)
    ).

-doc false.
-spec ensure_binary(term()) -> binary().
ensure_binary(Value) when is_binary(Value) -> Value;
ensure_binary(Value) when is_list(Value) -> list_to_binary(Value);
ensure_binary(Value) when is_atom(Value) -> atom_to_binary(Value, utf8).

-doc false.
-spec convert_pattern_properties(term()) -> map().
convert_pattern_properties(PatternProps) when is_map(PatternProps) ->
    maps:fold(
        fun(Pattern, SchemaAST, Acc) ->
            PatternBinary =
                case is_binary(Pattern) of
                    true -> Pattern;
                    false -> list_to_binary(Pattern)
                end,
            Schema = convert_type_definition(SchemaAST, [], []),
            Acc#{PatternBinary => Schema}
        end,
        #{},
        PatternProps
    );
convert_pattern_properties(_Other) ->
    #{}.

-doc false.
-spec convert_dependencies(term()) -> map().
convert_dependencies(Deps) when is_map(Deps) ->
    maps:fold(
        fun(Key, Value, Acc) ->
            KeyBinary = atom_to_binary(Key, utf8),
            DepValue =
                case is_list(Value) of
                    true ->
                        [atom_to_binary(V, utf8) || V <- Value];
                    false ->
                        convert_type_definition(Value, [], [])
                end,
            Acc#{KeyBinary => DepValue}
        end,
        #{},
        Deps
    );
convert_dependencies(_Other) ->
    #{}.
