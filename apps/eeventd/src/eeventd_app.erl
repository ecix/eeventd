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

start(_StartType, Args) ->
    io:format("Starting up with: ~p~n", [Args]),
    eed_broker_sup:start_link(),
    eed_redis_client:start_link(),
    eed_http_serv:start_link(),
    eeventd_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
