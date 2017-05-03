-module(eed_handler_sse).

-export([init/3]).
-export([info/3, terminate/3]).

init(_Type, Req, Opts) ->
    % Send initial 200 OK with text/event-stream as content type
    Headers = [
        {<<"Access-Control-Allow-Origin">>, <<"*">>},
        {<<"content-type">>, <<"text/event-stream">>}],
    {ok, Res} = cowboy_req:chunked_reply(200, Headers, Req),

    % Subscribe to events
    eed_broker:subscribe(),

    % Send initial keepalive
    erlang:send_after(1000, self(), keep_alive),
    {loop, Res, Opts}.


info(keep_alive, Req, State) ->
    % Send keep alive message
    cowboy_req:chunk(["keep-alive", "\n"], Req),

    % Trigger keepalive
    erlang:send_after(10000, self(), keep_alive),
    {loop, Req, State};


info({event, Sse}, Req, State) ->
    cowboy_req:chunk(Sse, Req),
    {loop, Req, State}.

terminate(_Reason, _Req, _State) -> ok.
