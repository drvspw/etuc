-module(eutc).

%% API Exports
-export([
         system_time/0,
         datetime/0,
         timestamp/0,
         now/0
        ]).

-on_load(init/0).

-define(APPNAME, ?MODULE).
-define(LIBNAME, "libeutc").

%%====================================================================
%% API functions - NIFS
%%====================================================================

%% @doc timestamp/0 utc time os:timestamp() format
%%
%% @end
-spec timestamp() -> Timestamp
                       when
    Timestamp :: os:timestamp().
timestamp() ->
  not_loaded(?LINE).

%% @doc datetime/0 utc time os:datetime() format
%%
%% @end
-spec datetime() -> DateTime
                       when
    DateTime :: calendar:datetime().
datetime() ->
  not_loaded(?LINE).

%% @doc system_time/0 utc system_time in nanoseconds
%%
%% @end
-spec system_time() -> System_Time
                       when
    System_Time :: integer().
system_time() ->
  not_loaded(?LINE).

%% @doc now/0 current UTC time
%%
%% @end
-spec now() -> #{seconds => Sec, nanos => Nanos, datetime => DateTime, iso8601 => DateTimeString}
                 when
    Sec :: integer(),
    Nanos :: integer(),
    DateTime :: calendar:datetime(),
    DateTimeString :: binary().
now() ->
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

eutc_test_() ->
  [
   ?_assertEqual(true, is_integer(eutc:system_time())),
   ?_assertEqual(true, is_map(eutc:now())),
   ?_assertMatch(#{datetime := {{_, _, _},{_, _, _}}}, eutc:now()),
   ?_assertMatch({{_, _, _},{_, _, _}}, eutc:datetime()),
   ?_assertMatch({_, _, _}, eutc:timestamp())
  ].

-endif.
