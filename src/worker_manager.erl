-module(worker_manager).

-behavior(gen_server).

%%
%% Exported Functions
%%

-export([cast/2, worker_loop/0, start/2, stop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%
%% API Functions
%%

cast(PID, Request) -> 
    gen_server:cast(PID, Request).

start(WorkerCount, WorkerFunction) ->
    io:format("Starting manager~n"),
    {ok, _PID} = gen_server:start_link(?MODULE, [WorkerCount, WorkerFunction], []).

stop() -> gen_server:cast(?MODULE, stop).

%%% Gen Server
init([WorkerCount, WorkerFun]) ->
    Fun =   fun (_Id) -> 
                PID = spawn(?MODULE, worker_loop, []),
%%                 io:format("Creating worker : ~p~n", [PID]),
                PID
            end,
    WorkerPids = lists:map(Fun, lists:seq(1, WorkerCount)),
    {ok, {WorkerFun, WorkerPids}}.

worker_loop() ->
    receive
        {worker_request, WorkerMFN, Request, From} ->
            io:format("Starting work : ~p / ~p~n", [WorkerMFN, Request]),
            WorkerMFN(Request),
            From ! {worker_done, self()},
            io:format("Finished work : ~p / ~p~n", [WorkerMFN, Request]),
            worker_loop();
        Other ->
            io:format("WTF, other? ~p~n", [Other])
    end.

handle_call(Request, _From, Workers) -> 
    RemainingWorkers = process_request(Request, Workers),
    {noreply, "Nothing", RemainingWorkers}.

handle_cast(Request, State) ->
    NewState = process_request(Request, State),
    {noreply, NewState}.

handle_info(_Info, State) -> 
    {noreply, State}. 

terminate(_Reason, _State) -> ok. 
code_change(_OldVsn, State, _Extra) -> {ok, State}. 

%%% Internal

process_request(Request, {WorkerMFN, [AvailableWorker | RemainingWorkers]}) ->
    io:format("Workers : ~p / ~p / ~p~n", [Request, AvailableWorker, RemainingWorkers]),
    AvailableWorker ! { worker_request, WorkerMFN, Request, self() },
    {WorkerMFN, RemainingWorkers};
process_request(Request, {WorkerMFN, []}) ->
    Workers = reclaim_workers(),
    process_request(Request, {WorkerMFN, Workers}).

reclaim_workers() ->
    reclaim_workers([]).

reclaim_workers(Workers) ->
    receive
        {worker_done, Pid} ->
            io:format("Worker done : ~p~n", [Pid]),
            reclaim_workers([ Pid | Workers ])
    after 10000 ->
            io:format("Done waiting, returning : ~p~n", [Workers]),
            Workers
    end.