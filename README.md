# gm_type_schema_converter

Erlang library for converting Erlang type definitions to JSON Schema format (compatible with OpenAPI 3.0.x).

## Purpose

This library provides a shared implementation for converting Erlang `-type` definitions to JSON Schema format. It is used by:

- **rebar3_openapi** plugin - For generating OpenAPI documentation from Erlang handlers
- **Runtime schema validation** - For validating request/response bodies against schemas

By sharing this logic, we avoid code duplication between the documentation generation tool and the runtime validation system.

## Features

- Converts all standard Erlang type AST patterns to JSON Schema
- Handles circular type references
- Supports required (`:=`) and optional (`=>`) map fields
- Capitalizes type names for schema naming (e.g., `user_profile` -> `UserProfile`)
- Supports user-defined type references and remote types
- Semantic type aliases via `gm_type` module (date, email, uuid, etc.)
- Constraint wrappers for numeric, string, array, and object validation
- Composition patterns: `anyOf`, `allOf`, `not`, discriminated unions
- OpenAPI metadata: description, title, example, deprecated, readOnly, writeOnly

## Installation

Add to your `rebar.config`:

```erlang
{deps, [
    {gm_type_schema_converter, {git, "git@github.com:greyorange-labs/gm_type_schema_converter.git", {branch, "main"}}}
]}.
```

## Usage

### Convert Single Type

```erlang
TypeDef = {user_id, {type, 0, binary, []}},
AllTypes = [TypeDef],
Schema = gm_type_schema_converter:type_to_schema(TypeDef, AllTypes).
%% Returns: #{<<"type">> => <<"string">>}
```

### Convert All Types

```erlang
Types = [
    {user_id, {type, 0, binary, []}},
    {user, {type, 0, map, [
        {type, 0, map_field_exact, [{atom, 0, id}, {user_type, 0, user_id, []}]},
        {type, 0, map_field_exact, [{atom, 0, name}, {type, 0, binary, []}]}
    ]}}
],
Schemas = gm_type_schema_converter:types_to_schemas(Types).
%% Returns: #{
%%     <<"UserId">> => #{<<"type">> => <<"string">>},
%%     <<"User">> => #{<<"type">> => <<"object">>, <<"properties">> => ...}
%% }
```

### Capitalize Type Name

```erlang
Name = gm_type_schema_converter:capitalize_type_name(user_profile).
%% Returns: <<"UserProfile">>
```

## Supported Type Conversions

### Primitive Types

| Erlang Type | JSON Schema |
|---|---|
| `binary()` | `{"type": "string"}` |
| `string()` | `{"type": "string"}` |
| `integer()` | `{"type": "integer"}` |
| `float()` | `{"type": "number"}` |
| `number()` | `{"type": "number"}` |
| `boolean()` | `{"type": "boolean"}` |
| `atom()` | `{"type": "string"}` |
| `term()` / `any()` | `{}` (accepts any value) |

### Integer Variants

| Erlang Type | JSON Schema |
|---|---|
| `non_neg_integer()` | `{"type": "integer", "minimum": 0}` |
| `pos_integer()` | `{"type": "integer", "minimum": 1}` |
| `neg_integer()` | `{"type": "integer", "maximum": -1}` |
| `byte()` | `{"type": "integer", "minimum": 0, "maximum": 255}` |
| `char()` | `{"type": "integer", "minimum": 0, "maximum": 1114111}` |
| `arity()` | `{"type": "integer", "minimum": 0, "maximum": 255}` |
| `0..150` (range) | `{"type": "integer", "minimum": 0, "maximum": 150}` |

### Literal Types

| Erlang Type | JSON Schema |
|---|---|
| `ok` (atom literal) | `{"type": "string", "enum": ["ok"]}` |
| `400` (integer literal) | `{"type": "integer", "enum": [400]}` |
| `$a` (char literal) | `{"type": "integer", "enum": [97]}` |
| `-1` (negative literal) | `{"type": "integer", "enum": [-1]}` |

### Collection Types

| Erlang Type | JSON Schema |
|---|---|
| `[ItemType]` | `{"type": "array", "items": {...}}` |
| `list()` | `{"type": "array"}` |
| `nonempty_list(T)` | `{"type": "array", "items": {...}, "minItems": 1}` |
| `[]` (nil) | `{"type": "array", "maxItems": 0}` |
| `maybe_improper_list(T, _)` | `{"type": "array", "items": {...}}` |
| `nonempty_improper_list(T, _)` | `{"type": "array", "items": {...}, "minItems": 1}` |
| `nonempty_string()` | `{"type": "string", "minLength": 1}` |
| `tuple()` | `{"type": "array"}` |
| `{A, B, C}` (typed tuple) | `{"type": "array", "minItems": 3, "maxItems": 3}` |

### Map Types

| Erlang Type | JSON Schema |
|---|---|
| `#{key := Value}` | `{"type": "object", "required": ["key"], "properties": {...}}` |
| `#{key => Value}` | `{"type": "object", "properties": {...}}` (optional) |
| `map()` | `{"type": "object"}` |

### Union Types

| Erlang Type | JSON Schema |
|---|---|
| `ok \| error` (all atoms) | `{"type": "string", "enum": ["ok", "error"]}` |
| `Type1 \| Type2` | `{"oneOf": [{...}, {...}]}` |

### Process and System Types

| Erlang Type | JSON Schema |
|---|---|
| `pid()` | `{"type": "string"}` |
| `port()` | `{"type": "string"}` |
| `reference()` | `{"type": "string"}` |
| `node()` | `{"type": "string"}` |
| `module()` | `{"type": "string"}` |
| `identifier()` | `{"type": "string"}` |
| `mfa()` | `{"type": "array", "minItems": 3, "maxItems": 3}` |
| `timeout()` | `{"oneOf": [{"type": "integer", "minimum": 0}, {"type": "string", "enum": ["infinity"]}]}` |

### Binary and IO Types

| Erlang Type | JSON Schema |
|---|---|
| `bitstring()` | `{"type": "string"}` |
| `<<_:N, _:_*M>>` | `{"type": "string"}` |
| `iodata()` | `{"type": "string"}` |
| `iolist()` | `{"type": "string"}` |

### Special Types

| Erlang Type | JSON Schema |
|---|---|
| `record()` | `{"type": "object"}` |
| `none()` / `no_return()` | `{"not": {}}` (impossible type) |
| `fun()` | `{}` (any, not serializable) |
| `Name :: Type` (annotated) | Converts the underlying type |
| Type variable (`T`) | `{}` (any) |
| `user_type()` | `{"$ref": "#/components/schemas/UserType"}` |
| `module:type()` | Resolved via type lookup |

## Semantic Types (gm_type module)

The `gm_type` module provides semantic type aliases that generate JSON Schema `format` keywords:

### String Formats

| gm_type | JSON Schema |
|---|---|
| `gm_type:date()` | `{"type": "string", "format": "date"}` |
| `gm_type:datetime()` | `{"type": "string", "format": "date-time"}` |
| `gm_type:email()` | `{"type": "string", "format": "email"}` |
| `gm_type:uri()` | `{"type": "string", "format": "uri"}` |
| `gm_type:uuid()` | `{"type": "string", "format": "uuid"}` |
| `gm_type:ipv4()` | `{"type": "string", "format": "ipv4"}` |
| `gm_type:ipv6()` | `{"type": "string", "format": "ipv6"}` |
| `gm_type:password()` | `{"type": "string", "format": "password"}` |
| `gm_type:base64()` | `{"type": "string", "format": "byte"}` |
| `gm_type:hostname()` | `{"type": "string", "format": "hostname"}` |

### Number Formats

| gm_type | JSON Schema |
|---|---|
| `gm_type:int32()` | `{"type": "integer", "format": "int32"}` |
| `gm_type:int64()` | `{"type": "integer", "format": "int64"}` |
| `gm_type:double()` | `{"type": "number", "format": "double"}` |

### Nullable Wrapper

| gm_type | JSON Schema |
|---|---|
| `gm_type:nullable(T)` | `{"oneOf": [T_schema, {"type": "null"}]}` |

## Constraint Wrappers

Constraint wrappers allow adding JSON Schema validation keywords to types:

```erlang
%% Float with min/max
-type temperature() :: {float_with_constraints, #{minimum => -273.15, maximum => 1000.0}}.

%% String with pattern and length
-type email_address() :: {binary_with_constraints, #{pattern => <<"^.+@.+$">>, max_length => 255}}.

%% Array with size limits
-type tags() :: {list_with_constraints, binary(), #{min_items => 1, max_items => 10, unique_items => true}}.

%% Object with property limits
-type config() :: {map_with_constraints, #{min_properties => 1}, #{name := binary(), value := binary()}}.
```

## Composition and Logic Constraints

```erlang
%% anyOf required: at least one of the listed keys must be present
-type search_filter() :: {any_of_required, [name, email, id], #{
    name => binary(), email => binary(), id => integer()
}}.

%% allOf required: all listed keys must be present
-type full_address() :: {all_of_required, {street, city, zip}, #{
    street => binary(), city => binary(), zip => binary(), country => binary()
}}.

%% not constraint: value must NOT match the negated schema
-type non_empty_string() :: {not_constraint, binary(), {type, 0, nil, []}}.

%% Discriminated union with propertyName
-type event() :: {discriminated_union, type, [click_event, scroll_event, submit_event]}.
```

## OpenAPI Metadata

Add OpenAPI documentation metadata to any type:

```erlang
-type user_id() :: {with_metadata, #{
    description => <<"Unique user identifier">>,
    title => <<"User ID">>,
    example => <<"usr_abc123">>,
    deprecated => false,
    read_only => true
}, gm_type:uuid()}.
```

## Development

```bash
make compile        # Compile the project
make test           # Run EUnit tests
make format         # Auto-format with erlfmt
make check-format   # Check formatting
make clean          # Clean build artifacts
```

## License

Apache-2.0
