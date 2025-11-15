# gm_type_schema_converter

Erlang library for converting Erlang type definitions to JSON Schema format (compatible with OpenAPI 3.0.x).

## Purpose

This library provides a shared implementation for converting Erlang `-type` definitions to JSON Schema format. It is used by:

- **rebar3_openapi** plugin - For generating OpenAPI documentation from Erlang handlers
- **Runtime schema validation** - For validating request/response bodies against schemas

By sharing this logic, we avoid code duplication between the documentation generation tool and the runtime validation system.

## Features

- Converts Erlang types (primitives, maps, lists, unions) to JSON Schema
- Handles circular references
- Supports required and optional map fields (`:=` vs `=>`)
- Capitalizes type names (e.g., `user_profile` → `UserProfile`)
- Supports type references and remote types

## Usage

### Convert Single Type

```erlang
TypeDef = {user_id, {type, 0, binary, []}},
AllTypes = [TypeDef],
Schema = gm_type_schema_converter:type_to_schema(TypeDef, AllTypes).
% Returns: #{<<"type">> => <<"string">>}
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
% Returns: #{
%     <<"UserId">> => #{<<"type">> => <<"string">>},
%     <<"User">> => #{<<"type">> => <<"object">>, <<"properties">> => ...}
% }
```

### Capitalize Type Name

```erlang
Name = gm_type_schema_converter:capitalize_type_name(user_profile).
% Returns: <<"UserProfile">>
```

## Installation

Add to your `rebar.config`:

```erlang
{deps, [
    {gm_type_schema_converter, {git, "git@github.com:yourorg/gm_type_schema_converter.git", {branch, "main"}}}
]}.
```

Or if published to hex.pm:

```erlang
{deps, [
    {gm_type_schema_converter, "~> 1.0.0"}
]}.
```

## Supported Type Conversions

| Erlang Type | JSON Schema |
|------------|-------------|
| `binary()` | `{type: "string"}` |
| `integer()` | `{type: "integer"}` |
| `float()` | `{type: "number"}` |
| `boolean()` | `{type: "boolean"}` |
| `#{key := Value}` | `{type: "object", required: ["key"], properties: {...}}` |
| `#{key => Value}` | `{type: "object", properties: {...}}` (optional) |
| `Type1 \| Type2` | `{oneOf: [{...}, {...}]}` |
| `[ItemType]` | `{type: "array", items: {...}}` |
| `user_type()` | `{$ref: "#/components/schemas/UserType"}` |

## License

Apache-2.0
