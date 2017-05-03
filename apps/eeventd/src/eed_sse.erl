
%
%
%
-module(eed_sse).
-export([encode/1, from_message/1]).


%%---------------------------------------------------------
%% @doc Transfrom incoming message from redis into SSE
%% @end
%%---------------------------------------------------------
from_message(Message) ->
    Data = jsx:encode(maps:get(<<"data">>, Message)),
    Event = maps:update(<<"data">>, Data, Message),
    encode(Event). 


encode({<<"data">>, Msg}) ->
    Lines = string:tokens(Msg, "\n"), 
    ["data: " ++ L || L <- Lines];

encode({<<"event">>, Event}) ->
    ["event: " ++ Event];

encode({<<"id">>, Id}) ->
    ["id: " ++ Id];

encode(Msg) ->
    compact([encode(M) || M <- maps:to_list(Msg)]).
 

compact([]) -> [];
compact([List|Rest]) ->
    List ++ compact(Rest). 

