
-module(eed_redis_client).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).

%% Callback
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {client}).



%%=========================================================
%% API
%%=========================================================

%%---------------------------------------------------------
%% @doc Start redis client
%% @end
%%---------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


stop() ->
    gen_server:stop(?SERVER).


%%=========================================================
%% Gen Server Callbacks
%%=========================================================
init([]) ->
    {ok, Client} = start_redis(), 
    fetch_queue(get_queue()),
    InitialState = #state{client=Client},
    {ok, InitialState}.
    

%%---------------------------------------------------------
%% @doc Nothing to call
%% @end
%%---------------------------------------------------------
handle_call(_Msg, _From, State) -> {noreply, State}.


%%---------------------------------------------------------
%% @doc Handle queue polling request
%% @end
%%---------------------------------------------------------
handle_cast({fetch_queue, Queue}, State) -> 
    case do_fetch_queue(State#state.client, Queue) of
        {ok, Payload} ->
            % Decode payload and dispatch event
            Message = jsx:decode(Payload, [return_maps]),
            dispatch_message(Message);
        {error, timeout} -> 
            % Do nothing
            ok
    end,

    fetch_queue(Queue),
    {noreply, State}.


handle_info(Msg, State) ->
    io:format("OOB MESSAGE: ~p~n", [Msg]),
    {noreply, State}.


terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%=========================================================
%% Queue polling and event fetching
%%=========================================================


%%---------------------------------------------------------
%% @doc Enqueue redis polling: Fetch data from queue
%% @end
%%---------------------------------------------------------
fetch_queue(Queue) ->
   gen_server:cast(?SERVER, {fetch_queue, Queue}). 



%%---------------------------------------------------------
%% @doc Fetch message from queue. Handle errors and
%%      timeouts.
%% @end
%%---------------------------------------------------------
do_fetch_queue(Client, Queue) ->
    case eredis:q(Client, ["BRPOP", Queue, 4]) of
        {ok, undefined} -> {error, timeout};
        {ok, [_Queue, Message]} -> {ok, Message}
    end.



%%---------------------------------------------------------
%% @doc Dispatch message received from redis,
%%      Extract event and data, encode sse payload
%%---------------------------------------------------------
dispatch_message(Message) ->
    Sse = eed_sse:from_message(Message),
    eed_broker:publish(Sse).





%%---------------------------------------------------------
%% @doc Get redis connection 
%% @end
%%---------------------------------------------------------
start_redis() ->
    % Get configuration
    Server = econfig:get_value(eeventd, "eventd", "redis_server"),
    [Host, ConfigPort] = string:tokens(Server, ":"),
    {Port, _} = string:to_integer(ConfigPort),
    Database = econfig:get_integer(eeventd, "eventd", "redis_database"),
   
    % Open redis connection
    eredis:start_link(Host, Port, Database).


%%---------------------------------------------------------
%% @doc Get polling queue from config
%% @end
%%---------------------------------------------------------
get_queue() ->
    Queue = econfig:get_value(eeventd, "eventd", "redis_queue"),
    Stream = econfig:get_value(eeventd, "eventd", "default_stream"),
    Queue ++ "_" ++ Stream.


