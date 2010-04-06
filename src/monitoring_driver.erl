-module(monitoring_driver).

%%
%% Include files
%%

%%
%% Exported Functions
%%
-export([main/1, parse_hosts/1]).

-export([find_ps/1, collect_stats/1, quick/1]).

%%
%% API Functions
%%

main(Args) -> 
	[HostFileName, BadHostFileName | _MoreArgs] = Args,
	{ok, BinaryHostData} = file:read_file(HostFileName),
	{ok, HostData} = parse_hosts(BinaryHostData),
	{ok, BinaryBadHostData} = file:read_file(BadHostFileName),
	{ok, BadHostData} = parse_hosts(BinaryBadHostData),
	{ok, BadHostFileHandle} = file:open(BadHostFileName, [write, append]),
	{IgnoredHosts, Hosts} = lists:partition(fun (Elem) -> 
						  lists:member(Elem, BadHostData) end, HostData),	

    io:format("Ignored Hosts : ~p~n", [IgnoredHosts]),
	io:format("Bad Hosts : ~p~n", [BadHostData]),
	io:format("Hosts to process : ~p~n", [Hosts]),
    
    PID = ssh_worker:start(8),
    lists:foreach(
        fun(Host) ->
            ssh_worker:remote_exec(PID, Host, {?MODULE, quick}) end,
        Hosts),
%%     receive_host_results(Hosts, BadHostFileHandle),
    receive_stats(Hosts, BadHostFileHandle),
	file:close(BadHostFileHandle).

receive_host_results(Hosts, HostFile) ->
    receive
        {host_results, Host, Results} ->
            io:format("Host ~p / Results ~p~n", [Host, Results]),
            receive_host_results(lists:delete(Host, Hosts), HostFile);
        {host_error, Reason, Host} ->
            io:format("Writing bad host to file : ~p / ~p~n", [Host, Reason]),
            io:format(HostFile, "~s~n", [Host]),
            receive_host_results(lists:delete(Host, Hosts), HostFile);
        Other ->
            io:format("Received other! ~p~n", [Other])
    after 300000 ->
        io:format("No results in 300 seconds and hosts aren't finished, remaining hosts : ~p~n", [Hosts])
    end.

receive_stats([], _BadHostFile) ->
    io:format("Done with hosts~n");

receive_stats(Hosts, BadHostFile) ->
    receive
        {Type, Results, Host} ->
            case {Type, Results, Host} of
%%                 {meminfo, Results, Host} ->
%%                     io:format("Host ~p / Results ~p~n", [Host, Results]);
%%                 {cpuinfo, Results, Host} ->
%%                     io:format("Host ~p / Results ~p~n", [Host, Results]);
%%                 {hostname, Results, Host} ->
%%                     io:format("Host ~p / Results ~p~n", [Host, Results]);
                {host_error, Reason, Host} ->
                    io:format("Writing bad host to file : ~p / ~p~n", [Host, Reason]),
                    io:format(BadHostFile, "~s~n", [Host]);
                {Stuff, Results, Host} -> io:format("Host ~p / ~p~n", [Stuff, Host])
            end,
            receive_stats(lists:delete(Host, Hosts), BadHostFile);
        Other ->
            io:format("Received other! ~p~n", [Other])
    after 60000 ->
        io:format("No results in 60 seconds and hosts aren't finished, remaining hosts : ~n~p~n", [Hosts])
    end.

quick({on_connect, Host, Args, _From}) ->
    {connect, Args, "yesmail", "ymQA@SC", [Host]};
quick({on_error, Reason, Host, From, _Context}) ->
    From ! {host_error, Reason, Host};
quick({connected, From, Host}) ->
    From ! {"done", "done", Host},
    {stop}.

collect_stats({on_connect, Host, Args, _From}) ->
    {connect, Args, "yesmail", "ymQA@SC", [Host]};
collect_stats({on_error, Reason, Host, From, _Context}) ->
    From ! {host_error, Reason, Host};
collect_stats({connected, _From, Host}) ->
    {execute, meminfo, "cat /proc/meminfo", Host};
collect_stats({meminfo, Results, From, Host}) ->
    From ! {meminfo, Results, Host},
    {execute, cpuinfo, "cat /proc/cpuinfo", Host};
collect_stats({cpuinfo, Results, From, Host}) ->
    From ! {cpuinfo, Results, Host},
    {execute, hostname, "hostname -f", Host};
collect_stats({hostname, Results, From, Host}) ->
    From ! {hostname, Results, Host},
    io:format("About to stop : ~p~n", [Host]),
    {stop}.

find_ps({on_connect, Host, Args, _From}) ->
	{connect, Args, "yesmail", "ymQA@SC", [Host]};
find_ps({connected, _From, Context}) ->
	{execute, ps, "ps -auxx | grep java", Context};
find_ps({ps, Results, _From, Context}) ->
 	{execute, ls, "ls -la", [Results|Context]};
find_ps({ls, Results, From, Context}) ->
    [Host | Remainder] = lists:reverse([Results | Context]),
    From ! {host_results, Host, [Remainder]},
	{stop};
find_ps({on_error, Reason, Host, From, _Context}) ->
    From ! {host_error, Reason, Host}.

parse_hosts(BinaryHostData) ->
	parse_hosts(BinaryHostData, [], []).

%%
%% Local Functions
%%

parse_hosts(<<$\n, Rest/binary>>, Line, Acc) -> 
	parse_hosts(Rest, [], [lists:reverse(Line)|Acc]);
parse_hosts(<<Char, Rest/binary>>, Line, Acc) -> 
	parse_hosts(Rest, [Char|Line], Acc);
parse_hosts(<<>>, _Line, Acc) ->
	{ok,lists:sort(Acc)}.
