%% DE-Bench: A benchmarking suite for distributed Erlang
%% This file is a modified version of basho_bench_sup.erl
%% For more information about the licence, please refer to: 
%% http://docs.basho.com/riak/latest/cookbooks/Benchmarking/
%% https://github.com/basho/basho_bench

%% Modified by: Amir Ghaffari <Amir.Ghaffari@glasgow.ac.uk>
%% RELEASE project (http://www.release-project.eu/)

-module(de_bench_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, workers/0, stop_child/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

-include("de_bench.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []). %% calls init synchronously

stop_child(Id) ->
    ok = supervisor:terminate_child(?MODULE, Id),
    ok = supervisor:delete_child(?MODULE, Id).

workers() ->
    [Pid || {_Id, Pid, worker, [de_bench_worker]} <- supervisor:which_children(?MODULE)].

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Sleep_time_bef=de_bench_config:get(sleep_time_before_ping, 0),
	timer:sleep(timer:seconds(Sleep_time_bef)), %% makes sure all nodes run Erlang VM

	case de_helper:get_S_Groups() of
	[] ->
		Erlang_nodes=de_bench_config:get(erlange_nodes, []);
	_GroupNames ->
		Transitive_connection=de_bench_config:get(transitive_connection, false),
		case Transitive_connection of
			false ->
				%% all commands, i.e. spawn, rpc, and name registration, are done just on nodes inside the current group
				Erlang_nodes=s_group:own_nodes(),
				?CONSOLE("transitive_connection is **false** and the number of nodes inside the current group is: ~p \n", [length(Erlang_nodes)]);
			_ ->
				%% Just name registration is done inside the current group, and other commands like spawn and rpc are done on all nodes in the cluster
				Erlang_nodes=de_bench_config:get(erlange_nodes, []),
				?CONSOLE("transitive_connection is **true** and the number of all connected nodes is: ~p \n", [length(Erlang_nodes)])
		end
	end,	

	Pangs=de_helper:ping_nodes(Erlang_nodes,[]),
	case Pangs of
	[] ->
		?CONSOLE("ping: All nodes (~p) are available \n", [length(Erlang_nodes)]);
	_->
		?ERROR("ping: ~p nodes from total ~p nodes are not accessible: ~p~n", [length(Pangs),length(Erlang_nodes), Pangs])
	end,

	%% initiate 
    Initial_global_size=de_bench_config:get(initial_global_size, 0),
    Initial_active_process=de_bench_config:get(initial_active_process, 0),
    ?CONSOLE("Initial global size = ~p ~n", [Initial_global_size]),
    ?CONSOLE("Initial active process = ~p ~n", [Initial_active_process]),
    initialize_global_size(Initial_global_size),
    initialize_active_process(Initial_active_process),
    %% find S_group
	case de_helper:get_S_Groups() of
	[] ->
		?CONSOLE("This node does not belong to any S_groups \n",[]);
	GroupNames ->
		?CONSOLE("This node belongs to these S_groups: ~p \n", [GroupNames])
	end,

	%% run workers 
    Workers = worker_specs(de_bench_config:get(concurrent), []),
    {ok, {{one_for_one, 100, 1}, %% terminates if 100 fails occur 1 second
		[?CHILD(de_bench_server, worker)] ++
		[?CHILD(de_bench_fsm, worker)] ++
		[?CHILD(de_bench_stats, worker)] ++
        Workers
    }}.

%% ===================================================================
%% Internal functions
%% ===================================================================

worker_specs(0, Acc) ->
    Acc;
worker_specs(Count, Acc) ->
    Id = list_to_atom(lists:concat(['worker_', Count])),
    Spec = {Id, {de_bench_worker, start_link, [Id, Count]},
            permanent, 5000, worker, [de_bench_worker]},
	worker_specs(Count-1, [Spec | Acc]).

%% Initially register a number of processes globally to see how the initial size of global name table affect the system performance
initialize_global_size(Initial_global_size) ->
case Initial_global_size of
	0 ->
		ok;
	_->
		ProcessName=de_helper:get_timestamp(),
		Pid=global:whereis_name(ProcessName),
		case is_pid(Pid) of
		true ->
			timer:sleep(1),
			initialize_global_size(Initial_global_size);
		false ->
			ProcessID=spawn(fun() -> idle_process() end), %% Create a process and register it in global name space
			PName=pid_to_list(ProcessID),
			global:register_name(list_to_atom(lists:append(PName, ProcessName)),ProcessID),
			initialize_global_size(Initial_global_size-1)
		end
end.

%% Idle processes are created to register their PID globally
idle_process() ->
	timer:seconds(5), %% sleep 5 second and become active again
	idle_process().

%% Initially run a number of active processes to see how number of active process affect the system performance
initialize_active_process(Initial_active_process) ->
	case Initial_active_process of
		0 ->
			ok;
		_->
			spawn(fun() -> active_process() end),
			initialize_active_process(Initial_active_process-1)
	end.


active_process() ->	
	de_helper:sleep_microsecond(10), %% keep busy for 10 microseconds
	active_process().






