
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
    Lines = string:tokens(binary:bin_to_list(Msg), "\n"), 
    ["data: " ++ L ++ "\n" || L <- Lines];

encode({<<"event">>, Event}) ->
    ["event: " ++ binary:bin_to_list(Event) ++ "\n"];

encode({<<"id">>, Id}) ->
    ["id: " ++ binary:bin_to_list(Id) ++ "\n"];

encode(Msg) ->
    compact([encode(M) || M <- maps:to_list(Msg)]) ++ ["\n\n"].
 

compact([]) -> [];
compact([List|Rest]) ->
    List ++ compact(Rest). 

