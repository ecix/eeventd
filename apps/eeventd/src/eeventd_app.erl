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

start(_StartType, _Args) ->

    % Register configuration
    ConfigFilename = get_config_filename(),
    io:format("Starting up with: ~p~n", [ConfigFilename]),
    ok = econfig:register_config(eeventd, [ConfigFilename]),

    % Startup application
    eed_broker_sup:start_link(),
    eed_redis_client_sup:start_link(),
    eed_http_serv:start_link(),
    eeventd_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================



%%---------------------------------------------------------
%% @doc Get config filename, passed via init:get_argument
%%      or via os:get_env.
%%
%% get_argument relies on -app-config passed at startup,
%% getenv uses the APP_CONFIG environment variable.
%% 
%% This raises an error if neither was defined.
%%---------------------------------------------------------
get_config_filename() ->
    EnvFilename = os:getenv("APP_CONFIG"),
    ArgsFilename = init:get_argument('app-config'),
    % Try cli argument, fall back to ENV
    Filename = case ArgsFilename of
            error -> EnvFilename;
            {ok, [[FArgs]]} -> FArgs
        end,
    % Okay we have our two options: Filename or false
    case Filename of
        false -> erlang:exit(app_config_missing);
        FEnv -> FEnv
    end.


