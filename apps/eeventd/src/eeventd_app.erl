%%%-------------------------------------------------------------------
%% @doc eeventd public API
%% @end
%%%-------------------------------------------------------------------

-module(eeventd_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    eed_broker_sup:start_link(),
    eeventd_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
