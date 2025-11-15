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
