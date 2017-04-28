
%
%
%
-module(eed_sse).
-export([encode/1, compact/1]).


encode({data, Msg}) ->
    Lines = string:tokens(Msg, "\n"), 
    ["data: " ++ L || L <- Lines];

encode({event, Event}) ->
    ["event: " ++ Event];

encode({id, Id}) ->
    ["id: " ++ Id];

encode(Msg) ->
    compact([encode(M) || M <- maps:to_list(Msg)]).
 

compact([]) -> [];
compact([List|Rest]) ->
    io:format("List: ~p~n", [List]), 
    List ++ compact(Rest). 

