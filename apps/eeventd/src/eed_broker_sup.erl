
-module(eed_broker_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Callbacks
-export([init/1]).


-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    ChildSpec = #{id => eed_broker,
                  start => {eed_broker, start_link, []},
                  type => worker},
                  
    {ok, { {one_for_all, 100, 600}, [ChildSpec]} }.


