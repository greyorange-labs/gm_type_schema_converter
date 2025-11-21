-module(gm_type_schema_converter).

%%%===================================================================
%%% Type to JSON Schema Converter
%%%===================================================================
%%%
%%% Converts Erlang type definitions (Abstract Syntax Trees) to
%%% JSON Schema format (compatible with OpenAPI 3.0.x).
%%%
%%% This library is shared between:
%%% - rebar3_openapi plugin (for documentation generation)
%%% - Runtime schema validation (for request/response validation)
%%%
%%%===================================================================

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

%% @doc Convert a single type definition to JSON Schema
%% @param TypeDef Tuple of {TypeName, TypeAST}
%% @param AllTypes List of all type definitions for reference resolution
%% @returns JSON Schema map
-spec type_to_schema(
    TypeDef :: type_def(),
    AllTypes :: [type_def()]
) -> json_schema().
type_to_schema({_TypeName, TypeDefAST}, AllTypes) ->
    %% Start with empty visited list - the type itself isn't circular on first visit
    convert_type_definition(TypeDefAST, AllTypes, []).

%% @doc Convert all type definitions to JSON Schemas map
%% @param Types List of type definitions
%% @returns Map of capitalized type names to JSON Schema maps
-spec types_to_schemas([type_def()]) -> #{binary() => json_schema()}.
types_to_schemas(Types) ->
    lists:foldl(
        fun({TypeName, _TypeDef} = TypeTuple, Acc) ->
            Schema = type_to_schema(TypeTuple, Types),
            %% Convert atom type name to capitalized binary for schema naming
            SchemaName = capitalize_type_name(TypeName),
            Acc#{SchemaName => Schema}
        end,
        #{},
        Types
    ).

%% @doc Capitalize type name for schema naming convention
%% Examples:
%% - user_profile -> <<"UserProfile">>
%% - user -> <<"User">>
%% - user_id -> <<"UserId">>
-spec capitalize_type_name(atom()) -> binary().
capitalize_type_name(TypeName) ->
    TypeStr = atom_to_list(TypeName),
    Words = string:split(TypeStr, "_", all),
    CapitalizedWords = lists:map(fun capitalize_word/1, Words),
    list_to_binary(string:join(CapitalizedWords, "")).

%%%===================================================================
%%% Internal Functions
%%%===================================================================

-spec convert_type_to_schema(
    {atom(), erl_parse:abstract_type()} | undefined,
    [type_def()],
    [atom()]
) -> json_schema().
convert_type_to_schema(undefined, _AllTypes, _Visited) ->
    #{<<"type">> => <<"object">>};
convert_type_to_schema({TypeName, TypeDef}, AllTypes, Visited) ->
    %% Check for circular references
    case lists:member(TypeName, Visited) of
        true ->
            %% Circular reference - return reference
            SchemaName = capitalize_type_name(TypeName),
            #{<<"$ref">> => <<"#/components/schemas/", SchemaName/binary>>};
        false ->
            convert_type_definition(TypeDef, AllTypes, [TypeName | Visited])
    end.

-spec convert_type_definition(
    erl_parse:abstract_type(),
    [type_def()],
    [atom()]
) -> json_schema().

%% Union type -> oneOf
convert_type_definition({type, _Line, union, Types}, AllTypes, Visited) ->
    Schemas = lists:map(
        fun(Type) -> convert_type_definition(Type, AllTypes, Visited) end,
        Types
    ),
    #{<<"oneOf">> => Schemas};

%% List type -> array
convert_type_definition({type, _Line, list, [ItemType]}, AllTypes, Visited) ->
    ItemsSchema = convert_type_definition(ItemType, AllTypes, Visited),
    #{
        <<"type">> => <<"array">>,
        <<"items">> => ItemsSchema
    };

%% Map with field definitions -> object with properties
%% This must come BEFORE the generic map pattern to match map fields first
convert_type_definition({type, _Line, map, Fields}, AllTypes, Visited) when is_list(Fields) ->
    %% Check if fields are map_field_* types (structured map) or just key/value types
    case is_map_with_fields(Fields) of
        true ->
            convert_map_with_fields(Fields, AllTypes, Visited);
        false ->
            %% Generic map with key/value types
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

%% Atom literal -> string enum
convert_type_definition({atom, _Line, Atom}, _AllTypes, _Visited) ->
    #{<<"type">> => <<"string">>, <<"enum">> => [atom_to_binary(Atom, utf8)]};

%% Remote type reference (e.g., module:type())
convert_type_definition({remote_type, _Line, [{atom, _, _Module}, {atom, _, TypeName}, []]}, AllTypes, Visited) ->
    %% Look up the type in AllTypes
    case find_type_definition(TypeName, AllTypes) of
        {TypeName, _TypeDef} = Found ->
            convert_type_to_schema(Found, AllTypes, Visited);
        undefined ->
            #{<<"type">> => <<"object">>}
    end;

%% User-defined type reference
convert_type_definition({user_type, _Line, TypeName, []}, AllTypes, _Visited) ->
    %% Always return a $ref for user-defined types to promote reusability
    %% The type should be converted separately at the top level
    case find_type_definition(TypeName, AllTypes) of
        {TypeName, _TypeDef} ->
            %% Type exists, return reference to it
            SchemaName = capitalize_type_name(TypeName),
            #{<<"$ref">> => <<"#/components/schemas/", SchemaName/binary>>};
        undefined ->
            %% Type not found, return reference anyway (assumes it will be defined)
            SchemaName = capitalize_type_name(TypeName),
            #{<<"$ref">> => <<"#/components/schemas/", SchemaName/binary>>}
    end;

%% anyOf required constraint: {any_of_required, [Key1, Key2, ...], BaseMapType}
%% Generates JSON Schema with anyOf array requiring at least one of the specified keys
convert_type_definition(
    {type, _Line, tuple, [
        {atom, _, any_of_required},
        KeyListAST,
        BaseMapType
    ]},
    AllTypes,
    Visited
) ->
    %% Extract list of required keys from AST
    RequiredKeys = extract_keys_from_list(KeyListAST),

    %% Convert base map type to JSON Schema
    BaseSchema = convert_type_definition(BaseMapType, AllTypes, Visited),

    %% Generate anyOf array with per-key required schemas
    AnyOfSchemas = generate_any_of_required_schemas(RequiredKeys),

    %% Add anyOf to base schema
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

%% Fallback for unknown types
convert_type_definition(_Other, _AllTypes, _Visited) ->
    #{<<"type">> => <<"object">>}.

%% @doc Check if a list contains map field definitions
-spec is_map_with_fields(list()) -> boolean().
is_map_with_fields([]) ->
    false;
is_map_with_fields([{type, _, map_field_exact, _} | _]) ->
    true;
is_map_with_fields([{type, _, map_field_assoc, _} | _]) ->
    true;
is_map_with_fields(_) ->
    false.

%% @doc Convert map with field definitions
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
                    NewReqAcc = case IsRequired of
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

%% @doc Convert generic map with key/value types
-spec convert_generic_map(
    list(),
    [type_def()],
    [atom()]
) -> json_schema().
convert_generic_map([_KeyType, ValueType], AllTypes, Visited) ->
    %% Map with key and value types -> object with additionalProperties
    ValueSchema = convert_type_definition(ValueType, AllTypes, Visited),
    #{
        <<"type">> => <<"object">>,
        <<"additionalProperties">> => ValueSchema
    };
convert_generic_map(_, _AllTypes, _Visited) ->
    %% Fallback for other map patterns
    #{<<"type">> => <<"object">>}.

%% @doc Convert a map field to {Key, Schema, IsRequired}
-spec convert_map_field(
    erl_parse:abstract_type(),
    [type_def()],
    [atom()]
) -> {binary(), json_schema(), boolean()} | {error, term()}.
convert_map_field({type, _Line, map_field_exact, [{atom, _, Key}, ValueType]}, AllTypes, Visited) ->
    %% Required field (uses :=)
    ValueSchema = convert_type_definition(ValueType, AllTypes, Visited),
    {atom_to_binary(Key, utf8), ValueSchema, true};
convert_map_field({type, _Line, map_field_assoc, [{atom, _, Key}, ValueType]}, AllTypes, Visited) ->
    %% Optional field (uses =>)
    ValueSchema = convert_type_definition(ValueType, AllTypes, Visited),
    {atom_to_binary(Key, utf8), ValueSchema, false};
convert_map_field(_Other, _AllTypes, _Visited) ->
    {error, invalid_field}.

%% @doc Find a type definition by name in the types list
-spec find_type_definition(atom(), [type_def()]) ->
    type_def() | undefined.
find_type_definition(TypeName, AllTypes) ->
    case lists:keyfind(TypeName, 1, AllTypes) of
        false -> undefined;
        Found -> Found
    end.

%% @doc Capitalize first letter of a word
-spec capitalize_word(string()) -> string().
capitalize_word([]) ->
    [];
capitalize_word([First | Rest]) ->
    [string:to_upper(First) | Rest].

%% @doc Extract atom keys from list or tuple AST
%% Handles Erlang abstract format for lists: {cons, Line, Head, Tail} or {nil, Line}
%% Also handles tuple format: {tuple, Line, [atom1, atom2, ...]}
-spec extract_keys_from_list(erl_parse:abstract_form()) -> [atom()].
extract_keys_from_list({cons, _Line, {atom, _, Key}, Rest}) ->
    [Key | extract_keys_from_list(Rest)];
extract_keys_from_list({nil, _Line}) ->
    [];
extract_keys_from_list({tuple, _Line, Elements}) ->
    %% Handle tuple format: {module_name, function_name, process_name}
    lists:map(
        fun({atom, _, Key}) -> Key;
           (Other) -> error({invalid_key_in_tuple, Other})
        end,
        Elements
    );
extract_keys_from_list({type, _Line, list, [{type, _Line2, union, Types}]}) ->
    %% Handle union type in list: [module_name | function_name | process_name]
    %% Extract atoms from union
    lists:map(
        fun({user_type, _, TypeName, []}) -> TypeName;
           ({atom, _, Atom}) -> Atom;
           (Other) -> error({invalid_union_type, Other})
        end,
        Types
    );
extract_keys_from_list(Other) ->
    error({invalid_key_list, Other}).

%% @doc Generate anyOf array with per-key required schemas
%% Each subschema requires exactly one of the specified keys
-spec generate_any_of_required_schemas([atom()]) -> [json_schema()].
generate_any_of_required_schemas(Keys) ->
    lists:map(
        fun(Key) ->
            KeyBinary = atom_to_binary(Key, utf8),
            #{<<"required">> => [KeyBinary]}
        end,
        Keys
    ).

%% @doc Generate allOf array with per-key required schemas
%% Each subschema requires one of the specified keys (all must be present)
-spec generate_all_of_required_schemas([atom()]) -> [json_schema()].
generate_all_of_required_schemas(Keys) ->
    lists:map(
        fun(Key) ->
            KeyBinary = atom_to_binary(Key, utf8),
            #{<<"required">> => [KeyBinary]}
        end,
        Keys
    ).

%% @doc Extract constraint map from AST
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

%% @doc Extract constraint value from AST
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
    %% For complex types (like schemas for contains, propertyNames), return the AST
    Other.

%% @doc Extract binary value from binary AST elements
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

%% @doc Add numeric constraints to schema (for floats)
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

%% @doc Add string constraints to schema
-spec add_string_constraints(json_schema(), map()) -> json_schema().
add_string_constraints(Schema, Constraints) ->
    lists:foldl(
        fun({Key, Value}, Acc) ->
            case Key of
                min_length -> Acc#{<<"minLength">> => Value};
                max_length -> Acc#{<<"maxLength">> => Value};
                pattern ->
                    Pattern = case is_binary(Value) of
                        true -> binary_to_list(Value);
                        false -> Value
                    end,
                    Acc#{<<"pattern">> => list_to_binary(Pattern)};
                format ->
                    Format = case is_binary(Value) of
                        true -> binary_to_list(Value);
                        false -> Value
                    end,
                    Acc#{<<"format">> => list_to_binary(Format)};
                _ -> Acc
            end
        end,
        Schema,
        maps:to_list(Constraints)
    ).

%% @doc Add array constraints to schema
-spec add_array_constraints(json_schema(), map()) -> json_schema().
add_array_constraints(Schema, Constraints) ->
    lists:foldl(
        fun({Key, Value}, Acc) ->
            case Key of
                min_items -> Acc#{<<"minItems">> => Value};
                max_items -> Acc#{<<"maxItems">> => Value};
                unique_items -> Acc#{<<"uniqueItems">> => Value};
                contains ->
                    %% Value should be a schema AST, convert it
                    ContainsSchema = convert_type_definition(Value, [], []),
                    Acc#{<<"contains">> => ContainsSchema};
                _ -> Acc
            end
        end,
        Schema,
        maps:to_list(Constraints)
    ).

%% @doc Add object constraints to schema
-spec add_object_constraints(json_schema(), map()) -> json_schema().
add_object_constraints(Schema, Constraints) ->
    lists:foldl(
        fun({Key, Value}, Acc) ->
            case Key of
                min_properties -> Acc#{<<"minProperties">> => Value};
                max_properties -> Acc#{<<"maxProperties">> => Value};
                property_names ->
                    PropNamesSchema = convert_type_definition(Value, [], []),
                    Acc#{<<"propertyNames">> => PropNamesSchema};
                pattern_properties ->
                    PatternProps = convert_pattern_properties(Value),
                    Acc#{<<"patternProperties">> => PatternProps};
                dependencies ->
                    Deps = convert_dependencies(Value),
                    Acc#{<<"dependencies">> => Deps};
                _ -> Acc
            end
        end,
        Schema,
        maps:to_list(Constraints)
    ).

%% @doc Convert pattern properties map to JSON Schema format
-spec convert_pattern_properties(term()) -> map().
convert_pattern_properties(PatternProps) when is_map(PatternProps) ->
    maps:fold(
        fun(Pattern, SchemaAST, Acc) ->
            PatternBinary = case is_binary(Pattern) of
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

%% @doc Convert dependencies map to JSON Schema format
-spec convert_dependencies(term()) -> map().
convert_dependencies(Deps) when is_map(Deps) ->
    maps:fold(
        fun(Key, Value, Acc) ->
            KeyBinary = atom_to_binary(Key, utf8),
            DepValue = case is_list(Value) of
                true ->
                    %% List of required keys
                    [atom_to_binary(V, utf8) || V <- Value];
                false ->
                    %% Schema dependency
                    convert_type_definition(Value, [], [])
            end,
            Acc#{KeyBinary => DepValue}
        end,
        #{},
        Deps
    );
convert_dependencies(_Other) ->
    #{}.
