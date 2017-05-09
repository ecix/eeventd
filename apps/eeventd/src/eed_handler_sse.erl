-module(eed_handler_sse).

-export([init/3, handle/2]).
-export([info/3, terminate/3]).

init(_Type, Req, Opts) ->
    % Setup headers
    Headers = [
        {<<"Access-Control-Allow-Origin">>, <<"*">>},
        {<<"content-type">>, <<"text/event-stream">>}],

    % Authenticate the client
    {Token, _} = cowboy_req:qs_val(<<"token">>, Req),

    case authenticate(Token) of
        token_missing ->
            TokenMissing = <<"Auth token missing.">>,
            {ok, Res} = cowboy_req:reply(403,
                                         Headers,
                                         TokenMissing,
                                         Req),
            {ok, Res, Opts};

        token_invalid ->
            TokenInvalid = <<"Auth token invalid.">>,
            {ok, Res} = cowboy_req:reply(403,
                                         Headers,
                                         TokenInvalid,
                                         Req),
            {ok, Res, Opts};
            
        ok -> 
            % Send initial 200 OK with text/event-stream as content type
            {ok, Res} = cowboy_req:chunked_reply(200, Headers, Req),

            % Subscribe to events
            eed_broker:subscribe(),

            % Send initial keepalive
            erlang:send_after(1000, self(), keep_alive),
            {loop, Res, Opts}
    end.


handle(Req, State) ->
    {ok, Req, State}.


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


%%---------------------------------------------------------
%% @doc Authenticate the client
%% @end
%%---------------------------------------------------------
authenticate(undefined) -> token_missing;

authenticate(Token) ->
    case validate_token(Token) of
        true -> ok;
        false -> token_invalid 
    end.
   

%%---------------------------------------------------------
%% @doc Validate the client token
%% @end
%%---------------------------------------------------------
validate_token(Token) when byte_size(Token) > 32 ->
    % Get Secret from config
    Secret = econfig:get_binary(eeventd, "security", secret_key),

    % Decode token and validate hash
    <<Hash:32/binary, Data/binary>> = base64:decode(Token),

    % Hash data with secret
    DataHash = crypto:hmac(sha256, Secret, Data),

    DataHash =:= Hash;


validate_token(_Token) ->
    false.
