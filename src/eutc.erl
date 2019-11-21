-module(eutc).

%% API Exports
-export([
         timestamp/0,
         now/0
        ]).

-on_load(init/0).

-define(APPNAME, ?MODULE).
-define(LIBNAME, "libeutc").

%%====================================================================
%% API functions - NIFS
%%====================================================================

%% @doc timestamp/0 utc timestamp in nanoseconds
%%
%% @end
-spec timestamp() -> Timestamp
                       when
    Timestamp :: integer().
timestamp() ->
  timestamp_nif().

%% @doc now/0 current UTC time
%%
%% @end
-spec now() -> #{seconds => Sec, nanos => Nanos, iso8601 => DateTimeString}
                 when
    Sec :: integer(),
    Nanos :: integer(),
    DateTimeString :: binary().
now() ->
  now_nif().

%%====================================================================
%% NIFS
%%====================================================================
timestamp_nif() ->
  not_loaded(?LINE).

now_nif() ->
  not_loaded(?LINE).

%%====================================================================
%%%% Internal functions
%%%%%====================================================================
init() ->
  PrivDir = code:priv_dir(?APPNAME),
  LibFile = lib_file(PrivDir, ?LIBNAME),
  erlang:load_nif(LibFile, 0).

lib_file({error, bad_name}, LibName) ->
  case filelib:is_dir(filename:join(["..", priv])) of
    true ->
      filename:join(["..", priv, LibName]);
    _ ->
      filename:join([priv, LibName])
  end;

lib_file(PrivDir, LibName) ->
  filename:join(PrivDir, LibName).


not_loaded(Line) ->
  exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).

%%=========================================================================
%% Unit Test Suite
%%=========================================================================
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

timestamp_test_() ->
  [
   ?_assertEqual(true, is_integer(eutc:timestamp())),
   ?_assertEqual(true, is_map(eutc:now()))
  ].

-endif.
