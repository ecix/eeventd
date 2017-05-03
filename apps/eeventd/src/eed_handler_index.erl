
-module(eed_handler_index).

-export([init/3, handle/2, terminate/3]).

init(_Type, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    Headers = [{<<"content-type">>, <<"text/plain">>}],
    Body = <<"eeventd v.0.1.0">>,
    {ok, Res} = cowboy_req:reply(200, Headers, Body, Req),
    {ok, Res, State}.

terminate(_Reason, _Req, _State) ->
    ok.

        

