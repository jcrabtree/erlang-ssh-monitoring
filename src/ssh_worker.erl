-module(ssh_worker).

-import(proplists, [property/2]).

%%
%% Exported Functions
%%
-export([remote_exec/3, start/1, stop/0]).

%%
%% API Functions
%%

remote_exec(PID, Host, ExecMFN) -> 
	worker_manager:cast(PID, {Host, ExecMFN, self()}).

stop() ->
    worker_manager:stop().

start(WorkerCount) ->
    io:format("Starting ssh worker~n"),
    case application:start(crypto) of
        ok -> 
            %% Started
            io:format("Started crypto~n");
        {error, {already_started, _}} -> 
            %% Already loaded, skip
            io:format("Skipping crypto~n")
    end,

    case application:start(ssh) of
        ok -> 
            %% Started
            io:format("Started SSH~n");
        {error, {already_started, _}} -> 
            %% Already loaded, skip
            io:format("Skipping SSH~n")
    end,

    {ok, PID} = worker_manager:start(WorkerCount, 
        fun({Host, ExecMFN, From}) ->
            execute_loop(Host, ExecMFN, From)
        end),

    io:format("SSH worker up : ~p~n", [PID]), 
    PID.
%%
%% Local Functions
%%
connect(Host, Args) ->
    SshArgs = ssh:connect(Host, Args),
	case SshArgs of
		{ok, ConnectionRef} ->
			{success, ConnectionRef};
		{error, ReasonList} when is_list(ReasonList) ->
			{failure, {unknown_list, ReasonList}};
		{error, timeout} ->
			{failure, ssh_timeout}
	end.

execute_loop(Host, ExecMFN, From) ->
	DefaultArgs = [{connect_timeout, 30000}, {user_interaction, false}, {silently_accept_hosts, true}],
    
	{connect, UserArgs, User, Password, Context} = ExecMFN({on_connect, Host, DefaultArgs, From}),
	Args = [{user, User}, {password, Password} | UserArgs],
	case connect(Host, Args) of
		{success, ConnectionRef} ->
			State = ExecMFN({connected, From, Context}),
			connected_loop(ConnectionRef, ExecMFN, Host, State, From),
			ssh:close(ConnectionRef);
		{failure, Reason} ->
			ExecMFN({on_error, Reason, Host, From, Context});
        {error, Reason} ->
            ExecMFN({on_error, Reason, Host, From, Context})
	end.

connected_loop(ConnectionRef, ExecMFN, Host, State, From) ->
	case State of
		{execute, Id, Command, Context} ->
            case ssh_connection:session_channel(ConnectionRef, 30000) of 
                {ok, ChannelId} -> 
                    ssh_connection:exec(ConnectionRef, ChannelId, Command, 30000),
                    Results = exec_receive_loop(ChannelId, []),
                    NextState = ExecMFN({Id, Results, From, Context}),
                    connected_loop(ConnectionRef, ExecMFN, Host, NextState, From);
                {error, Reason} ->
                    ExecMFN({on_error, Reason, Host, From, Context}) 
            end;
		{stop} ->
            stop;
        Other ->
            io:format("WTF?  Other? ~p~n", [Other])
	end.

exec_receive_loop(ChannelId, Collector) ->
	receive
  		{ssh_cm, _ConnectionRef, {data, _Channel, _TypeCode, Data}} ->
			exec_receive_loop(ChannelId, string:concat(Collector, binary_to_list(Data)));
		{ssh_cm, _ConnectionRef, {closed, _Channel}} ->
			Collector;
  		{ssh_cm, _ConnectionRef, _} ->
			exec_receive_loop(ChannelId, Collector)
	after 30000 ->
		Collector
	end.