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

%% @doc iso8601/0 current UTC time as RFC 3339 and ISO 8601 formatted string
%%
%% @end
-spec now() -> DateTimeString
        when
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
    SoName = case code:priv_dir(?APPNAME) of
       {error, bad_name} ->
          case filelib:is_dir(filename:join(["..", priv])) of
            true ->
              filename:join(["..", priv, ?LIBNAME]);
            _ ->
              filename:join([priv, ?LIBNAME])
          end;
       Dir ->
          filename:join(Dir, ?LIBNAME)
    end,
    erlang:load_nif(SoName, 0).

not_loaded(Line) ->
            exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).

%%=========================================================================
%% Unit Test Suite
%%=========================================================================
-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

timestamp_test_() ->
  [
   ?_assertEqual(1, eutc:now())
  ].

-endif.
