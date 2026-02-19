# Plan: Add Missing JSON Schema / OpenAPI Type Support

## Context

`gm_type_schema_converter` converts Erlang type ASTs to JSON Schema (OpenAPI 3.0.x). The generated schemas are used in two ways:
1. **OpenAPI documentation** — via `rebar3_openapi` plugin
2. **Runtime request body validation** — via `gm_schema_validation.erl` in `butler_server`, which uses **jesse** (JSON Schema Draft-06 validator)

The library currently handles primitives, maps, lists, unions, atom literals, integer ranges, constraint wrappers, and composition keywords. Many common Erlang types, JSON Schema formats, and OpenAPI metadata features are missing.

### Jesse Compatibility Constraints

jesse supports **Draft-03/04/06 only**. This constrains what we can generate:

| Feature | jesse validates? | Strategy |
|---|---|---|
| type, enum, const | Yes | Use freely |
| allOf/anyOf/oneOf/not | Yes | Use freely |
| All numeric/string/array/object constraints | Yes | Use freely |
| `nullable: true` | No | Use `oneOf: [schema, {type: null}]` instead |
| `format` (date-time, email, ipv4, ipv6) | Yes validated | Use freely |
| `format` (date, uuid, hostname, uri, etc.) | No silently passes | Generate for docs; register custom validators or fork jesse for validation |
| `if/then/else` | No Draft-07 | Requires jesse fork |
| `discriminator` | No OpenAPI ext | For docs only; no validation effect |
| `prefixItems` | No Draft-2020-12 | Requires jesse fork |
| `description/title/example/default/deprecated` | No annotations | jesse ignores; useful for docs only |
| `readOnly/writeOnly` | No annotations | jesse ignores; useful for docs only |

### Critical files

- **Converter**: `src/gm_type_schema_converter.erl`
- **Types module**: `src/gm_type.erl`
- **Tests**: `test/gm_type_schema_converter_tests.erl`
- **App spec**: `src/gm_type_schema_converter.app.src`

---

## Phase 1: Built-in Erlang Types + Enum Optimization [DONE]

- Atom-union to single enum optimization (all-atom unions collapse to `{type: string, enum: [...]}`)
- Mixed unions collapse atoms into one enum entry within `oneOf`
- Added: `non_neg_integer()`, `pos_integer()`, `neg_integer()`, `number()`, `nonempty_list(T)`, `string()`, `byte()`, `char()`, `arity()`, `atom()`, `term()`, `any()`, `timeout()`, `tuple()` (generic), specific tuples
- Logger warning for unknown types
- `partition_atoms_and_types/1` helper

## Phase 2: Internal Types Module (gm_type) [DONE]

- Created `src/gm_type.erl` with semantic type aliases
- String formats: date, datetime, email, uri, uuid, ipv4, ipv6, password, base64, hostname
- Number formats: int32, int64, double
- `gm_type:nullable(T)` -> `{oneOf: [T, {type: null}]}` (jesse-compatible)

## Phase 3: OpenAPI Metadata Features [DONE]

- `with_metadata` wrapper for description, title, example, default, deprecated, readOnly, writeOnly
- `additional_properties` support in `map_with_constraints`
- `add_metadata/2` and `ensure_binary/1` helpers

## Phase 4: Advanced Features [DONE]

- `discriminated_union` for tagged unions with discriminator property
- `extract_variant_types/1` helper

---

## Verification

All phases verified with:
1. `rebar3 compile` — no compilation errors or warnings
2. `rebar3 eunit` — 74 tests, 0 failures
