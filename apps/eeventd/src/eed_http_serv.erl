
-module(eed_http_serv).
-behaviour(gen_server).

% Api
-export([start_link/0]).

% Gen Server
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

-define(SERVER, ?MODULE).

%%=========================================================
%% API
%%=========================================================


%%---------------------------------------------------------
%% @doc Start the HTTP server
%% @end
%%---------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).



%%=========================================================
%% Gen Server Callbacks
%%=========================================================


%%---------------------------------------------------------
%% @doc Initialize http server
%% @end
%%---------------------------------------------------------
init([]) ->
    % TODO Read config

    Dispatch = cowboy_router:compile([
        { '_', [
        ]}
    ]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 7117}], [
        {env, [{dispatch, Dispatch}]}
    ]),
    {ok, []}.


%%---------------------------------------------------------
%% @doc This server does not react to calls. 
%% @end
%%---------------------------------------------------------
handle_call(_Msg, _From, State) -> {noreply, State}.


%%---------------------------------------------------------
%% @doc This server does not handle casts either.
%% @end
%%---------------------------------------------------------
handle_cast(_Msg, State) -> {noreply, State}.


%%---------------------------------------------------------
%% @doc This server does not handle oob messages.
%% @end
%%---------------------------------------------------------
handle_info(_Msg, State) -> {noreply, State}.


%%---------------------------------------------------------
%% @doc Code upgrade path
%% @end
%%---------------------------------------------------------
code_change(_OldSvn, State, _Extra) -> {ok, State}.


%%---------------------------------------------------------
%% @doc Terminate server
%% @end
%%---------------------------------------------------------
terminate(normal, _State) -> ok.
