
-module(eed_redis_client_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    ChildSpec = #{id => eed_redis_client,
                  start => {eed_redis_client, start_link, []},
                  type => worker},
    {ok, {{one_for_all, 100, 600}, [ChildSpec]}}.



