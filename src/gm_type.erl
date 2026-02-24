%%%-------------------------------------------------------------------
%%% @author Platform Team <butler_server_platform@greyorange.sg>
%%% @copyright (C) 2025, Grey Orange
%%% @doc
%%% Semantic Type Aliases for JSON Schema / OpenAPI Format Generation
%%%
%%% These types alias standard Erlang types but are recognized by
%%% gm_type_schema_converter to generate JSON Schema `format` keywords.
%%%
%%% Usage in type specs:
%%%   -type created_at() :: gm_type:datetime().
%%%   -type user_email() :: gm_type:email().
%%%   -type user_id() :: gm_type:uuid().
%%% @end
%%%-------------------------------------------------------------------
-module(gm_type).

-export_type([
    %% String formats
    date/0,
    datetime/0,
    email/0,
    uri/0,
    uuid/0,
    ipv4/0,
    ipv6/0,
    password/0,
    base64/0,
    hostname/0,
    %% Number formats
    int32/0,
    int64/0,
    double/0,
    %% Nullable wrapper
    nullable/1
]).

%% String format types
-type date() :: binary().
-type datetime() :: binary().
-type email() :: binary().
-type uri() :: binary().
-type uuid() :: binary().
-type ipv4() :: binary().
-type ipv6() :: binary().
-type password() :: binary().
-type base64() :: binary().
-type hostname() :: binary().

%% Number format types
-type int32() :: integer().
-type int64() :: integer().
-type double() :: float().

%% Nullable wrapper
-type nullable(T) :: T | undefined.
