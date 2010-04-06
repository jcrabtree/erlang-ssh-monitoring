%% Author: mreynolds
-module(meminfo).

-import(proplists, [property/2]).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([main/1, parse/1]).

%%
%% API Functions
%%

main(_Args) -> 
    {ok, Data} = file:read_file("samples/meminfo"),
    RawData = parse(Data),
    io:format("Data ~n~p~n", [RawData]).
    
parse(Data) ->
    parse(Data, [], []).

parse(<<":", Rest/binary>>, Buffer, Acc) -> 
    Term = lists:reverse(Buffer),
    parse(Rest, [], [Term | Acc]);

parse(<<$\n, Rest/binary>>, Buffer, [TermName | Acc]) ->
    Data = string:strip(lists:reverse(Buffer)),
    parse(Rest, [], [{TermName, Data} | Acc]);

parse(<<Char, Rest/binary>>, Buffer, Acc) ->
    parse(Rest, [Char|Buffer], Acc);

parse(<<>>, _Value, Acc) ->
    Acc.