-module(eutc).

%% API Exports
-export([
	 timestamp/0,
	 iso8601/0,
	 rfc3339/0
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
-spec iso8601() -> DateTimeString
	when
	DateTimeString :: binary().
iso8601() ->
	iso8601_nif().

%% @doc iso8601/0 current UTC time as RFC 3339 and ISO 8601 formatted string
%%
%% @end
-spec rfc3339() -> DateTimeString
	when
	DateTimeString :: binary().
rfc3339() ->
	iso8601().

%%====================================================================
%% NIFS
%%====================================================================
timestamp_nif() ->
    not_loaded(?LINE).

iso8601_nif() ->
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
