%% Author: mreynolds
-module(cpuinfo).

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
    {ok, Data} = file:read_file("cpuinfo"),
    CpuData = parse(Data),
    lists:foreach(fun ({cpuinfo, Item}) ->
                      Processor = proplists:get_value("processor", Item),
                      MHz = proplists:get_value("cpu MHz", Item),
                      io:format("Proc / MHz : ~p/~p~n", [Processor, MHz])
                    end, CpuData).                
    
parse(Data) ->
    parse(Data, [], [], []).

parse(<<"\t: ", Rest/binary>>, Buffer, Acc, Procs) -> 
    Term = lists:reverse(Buffer),
    parse(Rest, [], [Term | Acc], Procs);

parse(<<$\t, Rest/binary>>, Buffer, Acc, Procs) -> 
     parse(Rest, Buffer, Acc, Procs);

parse(<<$\n, Rest/binary>>, [], Acc, Procs) ->
     parse(Rest, [], [], [{cpuinfo, Acc} | Procs]);
    
parse(<<$\n, Rest/binary>>, Buffer, [TermName | Acc], Procs) ->
     parse(Rest, [], [{TermName, lists:reverse(Buffer)} | Acc], Procs);

parse(<<Char, Rest/binary>>, Buffer, Acc, Procs) ->
    parse(Rest, [Char|Buffer], Acc, Procs);

parse(<<>>, _Value, Acc, Procs) ->
    io:format("Done ~p~n", [Acc]),
    Procs.