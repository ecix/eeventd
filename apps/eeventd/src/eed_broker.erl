
-module(eed_broker).

-behaviour(gen_server).

-export([start_link/0,
         publish/1,
         subscribe/0,
         subscribe/1,
         unsubscribe/0,
         unsubscribe/1]).

-export([init/1,
         handle_cast/2,
         handle_call/3,
         handle_info/2,
         terminate/2,
         code_change/3]).


-define(SERVER, ?MODULE).

-record(state, {subscriptions}).


%%=========================================================
%% API
%%=========================================================

%%---------------------------------------------------------
%% @doc Start the broker
%% @end
%%---------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


%%---------------------------------------------------------
%% @doc Let an eventsource publish a message 
%% @end
%%---------------------------------------------------------
publish(Msg) ->
    gen_server:cast(?SERVER, {publish, Msg}).



%%---------------------------------------------------------
%% @doc Subscribe to hub 
%% @end
%%---------------------------------------------------------
subscribe() ->
    subscribe(self()).


%%---------------------------------------------------------
%% @doc Register listener
%% @end
%%---------------------------------------------------------
subscribe(Listener) ->
   gen_server:cast(?SERVER, {subscribe, Listener}).


%%---------------------------------------------------------
%% @doc Remove listener
%% @end
%%---------------------------------------------------------
unsubscribe(Listener) ->
    gen_server:cast(?SERVER, {unsubscribe, Listener}).


%%---------------------------------------------------------
%% @doc Remove current listener
%% @end
%%---------------------------------------------------------
unsubscribe() ->
    unsubscribe(self()).



%%=========================================================
%% Gen Server Implementation 
%%=========================================================

%%---------------------------------------------------------
%% @doc Initialize message hub
%% @end
%%---------------------------------------------------------
init([]) ->
    InitialState = #state{subscriptions=sets:new()},
    {ok, InitialState}.


%%---------------------------------------------------------
%% @doc Handle calls (we do not have any.) 
%% @end
%%---------------------------------------------------------
handle_call(_Msg, _From, State) ->
    {noreply, State}.



%%---------------------------------------------------------
%% @doc Handle subscribe to broker:
%%      Add subscriber to listeners set. Monitor the
%%      listener and unsubscribe when it is gone.
%% @end
%%---------------------------------------------------------
handle_cast({subscribe, Listener}, State) ->
    Subscriptions = do_subscribe(Listener, State#state.subscriptions),
    NextState = State#state{subscriptions = Subscriptions},
    {noreply, NextState};


%%---------------------------------------------------------
%% @doc Handle unsubscribe to broker:
%%      Remove the listener from subscriptions set.
%%      De-Monitor the listener.
%% @end
%%---------------------------------------------------------
handle_cast({unsubscribe, Listener}, State) ->
    Subscriptions = do_unsubscribe_listener(Listener, State#state.subscriptions),
    NextState = State#state{subscriptions = Subscriptions},
    {noreply, NextState};


%%---------------------------------------------------------
%% @doc Handle event publishing:
%       Send event to all subscribers. 
%% @end
%%---------------------------------------------------------
handle_cast({publish, Msg}, State) ->
    do_publish(Msg, State#state.subscriptions),
    {noreply, State}.



%%---------------------------------------------------------
%% @doc Handle gone subscriber
%% @end
%%---------------------------------------------------------
handle_info({'DOWN', Ref, process, Listener, _Reason}, State) -> 
    Subscriptions = do_unsubscribe_subscription({Ref, Listener},
                                                State#state.subscriptions),
    NextState = State#state{subscriptions = Subscriptions},
    {noreply, NextState}.

%%---------------------------------------------------------
%% @doc Handle termination 
%% @end
%%---------------------------------------------------------
terminate(normal, _State) ->
    ok.

%%---------------------------------------------------------
%% @doc Handle code change 
%% @end
%%---------------------------------------------------------
code_change(_OldSvn, State, _Extra) ->
    {ok, State}.

%%=========================================================
%% Broker Implementation 
%%=========================================================


%%---------------------------------------------------------
%% @doc Subscribe to broker 
%% @end
%%---------------------------------------------------------
do_subscribe(Listener, Listeners) ->
    Ref = erlang:monitor(process, Listener),
    sets:add_element({Ref, Listener}, Listeners).



%%---------------------------------------------------------
%% @doc Listener leave broker, without subscription
%% @end
%%---------------------------------------------------------
do_unsubscribe_listener(Listener, Listeners) ->
    Subscriptions = sets:to_list(maps:filter(fun(S) ->
        case S of
            {_Ref, Listener} -> true;
            _ -> false
        end
    end, Listeners)),
    [do_unsubscribe_subscription(S, Listeners) || S <- Subscriptions].



%%---------------------------------------------------------
%% @doc Leave broker with subscription
%% @end
%%---------------------------------------------------------
do_unsubscribe_subscription(Subscription, Listeners) ->
    sets:del_element(Subscription, Listeners).


%%---------------------------------------------------------
%% @doc Publish an event to all listeners
%% @end
%%---------------------------------------------------------
do_publish(Event, Listeners) ->
    [L ! {event, Event} || {_Ref, L} <- sets:to_list(Listeners)].

