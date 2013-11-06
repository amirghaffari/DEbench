%% DE-Bench: A benchmarking suite for distributed Erlang
%% This file is a modified version of basho_bench_worker.erl
%% For more information about the licence, please refer to: 
%% http://docs.basho.com/riak/latest/cookbooks/Benchmarking/
%% https://github.com/basho/basho_bench

%% Modified by: Amir Ghaffari <Amir.Ghaffari@glasgow.ac.uk>
%% RELEASE project (http://www.release-project.eu/)

-module(de_bench_worker).

-behaviour(gen_server).

%% API
-export([start_link/2, run/1]).

%% gen_server callbacks
-export([init/1,terminate/2, handle_info/2, handle_call/3, handle_cast/2, code_change/3]).

-record(state, { id, sup_id, worker_pid, parent_pid, ops, ops_len, next_op, data, erlang_nodes, continue, s_groups}).

-include("de_bench.hrl").

%% ====================================================================
%% API
%% ====================================================================

run(Pids) ->
    [ok = gen_server:call(Pid, run) || Pid <- Pids],
    ok.

start_link(SupChild, Id) ->
    gen_server:start_link(?MODULE, [SupChild, Id], []).

%% ====================================================================
%% gen_server callbacks
%% ====================================================================


init([SupChild, Id]) ->
	case de_helper:get_S_Groups() of
	[] ->
		S_Groups=false,
		Erlang_nodes=de_bench_config:get(erlange_nodes, []);
	_GroupNames ->
		S_Groups=true,
		Transitive_connection=de_bench_config:get(transitive_connection, false),
		case Transitive_connection of
			false ->
				%% all commands, i.e. spawn, rpc, and name registration, are done just on nodes inside the current group
				Erlang_nodes=lists:delete(node(), s_group:own_nodes()),
				?CONSOLE("transitive_connection is **false** and the number of nodes inside the current group except ~p is: ~p \n", [node(),length(Erlang_nodes)]);
			_ ->
				%% Just name registration is done inside the current group
				%% remove the current node from the target node list
				Erlang_nodes=lists:delete(node(), de_bench_config:get(erlange_nodes, [])),
				?CONSOLE("transitive_connection is **true** and the number of all connected nodes is: ~p \n", [length(Erlang_nodes)])
		end
	end,

    case Erlang_nodes of
        [] ->
			?FAIL_MSG("~s requires erlange_nodes to be defined in config file.\n", [?MODULE]);
        _ ->
            ok
    end,
	Ops     = ops_tuple(),
	State = #state { id = Id, sup_id = SupChild, parent_pid=self(), ops = Ops, ops_len = size(Ops), next_op='', data='', erlang_nodes=Erlang_nodes, continue=true, s_groups=S_Groups},

	%% NOTE: If the worker process dies, this obviously introduces some entroy
	%% into the equation since you'd be restarting the RNG all over.
	process_flag(trap_exit, true),
   %% Link the worker and the sub-process to ensure that if either exits, the
    %% other goes with it.
    WorkerPid = spawn_link(fun() -> worker_init(State) end),
    WorkerPid ! {init_driver, self()},
    receive
        driver_ready ->
        ok
    end,

    %% If the system is marked as running this is a restart; queue up the run
    %% message for this worker
    case de_bench_app:is_running() of
        true ->
            ?WARN("Restarting crashed worker.\n", []),
            gen_server:cast(self(), run);
        false ->
            ok
    end,
	{ok, State#state { worker_pid = WorkerPid}}.

terminate(_Reason, _State) ->
    ok.

handle_call(run, _From, State) ->
	State#state.worker_pid ! run,
	{reply, ok, State}. 

handle_cast(run, State) ->
	State#state.worker_pid ! run,
	{noreply, State}.

handle_info({'EXIT', Pid, Reason}, State) ->
    case Reason of
        normal ->
            %% Clean shutdown of the worker; spawn a process to terminate this
            %% process via the supervisor API and make sure it doesn't restart.
            spawn(fun() -> stop_worker(State#state.sup_id) end),
            {noreply, State};

        _ ->
            ?ERROR("Worker ~p exited with reason: ~p~n", [Pid, Reason]),
            %% Worker process exited for some other reason; stop this process
            %% as well so that everything gets restarted by the sup
            {stop, normal, State}
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================


worker_init(State) ->
    %% Trap exits from linked parent process; use this to ensure the driver
    %% gets a chance to cleanup
    process_flag(trap_exit, true),
    worker_idle_loop(State).


worker_idle_loop(State) ->
    receive
        {init_driver, Caller} ->
			Caller ! driver_ready,
            worker_idle_loop(State);
        run ->
			io:format("Worker process ~p starts at ~p ~n",[State#state.id, erlang:time()]),
			worker_active_loop(State)
    end.

needs_shutdown(State) ->
    Parent = State#state.parent_pid,
    receive
        {'EXIT', Parent, _Reason} ->
            true
    after 0 ->
		case State#state.continue of
			true ->
				false;
			false ->
				true
		end
    end.

worker_active_loop(State) ->
	case State#state.next_op of
	global_unregister ->
		Next = {global_unregister, global_unregister};
	global_whereis ->
		Next = {global_whereis, global_whereis};
	local_unregister ->
		Next = {local_unregister, local_unregister};
	local_whereis ->
		Next = {local_whereis, local_whereis};
	_ ->
		Next = element(de_helper:getRandomm(State#state.ops_len), State#state.ops)
	end,
    {_Label, OpTag} = Next,
	Erlang_nodes  = State#state.erlang_nodes,
	NodeIndex = de_helper:getRandomm(length(Erlang_nodes)),
	case is_list(lists:nth(NodeIndex,Erlang_nodes)) of
	true ->
		Selected_node = list_to_atom(lists:nth(NodeIndex,Erlang_nodes));
	false ->
		Selected_node = lists:nth(NodeIndex,Erlang_nodes)
	end,
	case node() =:= Selected_node of
	true when Selected_node /= 'nonode@nohost' ->
		OpTag2=local_node; % local
	_ -> 
		OpTag2=OpTag
	end,

	case OpTag2 of
	local_node ->
		State2=State;
	global_unregister when State#state.next_op /= global_unregister ->
		State2=State;
	global_whereis when State#state.next_op /= global_whereis ->
		State2=State;
	local_unregister when State#state.next_op /= local_unregister ->
		State2=State;
	local_whereis when State#state.next_op /= local_whereis ->
		State2=State;
	_ ->
		Result = (catch de_commands:run(Selected_node, OpTag, State#state.data, State#state.s_groups) ),
		case Result of 
			{ok, ElapsedUs, _} ->
				case register_results(Next, ok, ElapsedUs, 0) of
					ok ->
						State2=State#state {next_op='', data='', continue=true};
					finish ->
						State2=State#state {next_op='', data='', continue=false}
				end;

			{ok, ElapsedUs, global_register, Res} ->
				case register_results(Next, ok, ElapsedUs, 0) of
					ok ->
						State2=State#state {next_op=global_whereis, data=Res, continue=true};
					finish ->
						State2=State#state {next_op=global_whereis, data=Res, continue=false}
				end;

			{ok, ElapsedUs, global_whereis, Res} ->
				case register_results(Next, ok, ElapsedUs, 0) of
					ok ->
						State2=State#state {next_op=global_unregister, data=Res, continue=true};
					finish ->
						State2=State#state {next_op=global_unregister, data=Res, continue=false}
				end;

			{error, ElapsedUs, Reason} ->
				case register_results(Next, {error, Reason}, ElapsedUs, 0) of
					ok ->
						State2=State;
					finish ->
						State2=State#state {continue=false}
				end;

			{error, ElapsedUs, whereis_error, Data} ->
				case register_results(Next, {error, whereis_error}, ElapsedUs, 0) of
					ok -> 
						State2=State#state {next_op=global_unregister, data=Data, continue=true};
					finish ->
						State2=State#state {next_op=global_unregister, data=Data, continue=false}
				end;

			{error, ElapsedUs, local_whereis_error, Data} ->
				case register_results(Next, {error, local_whereis_error}, ElapsedUs, 0) of
					ok -> 
						State2=State#state {next_op=local_unregister, data=Data, continue=true};
					finish ->
						State2=State#state {next_op=local_unregister, data=Data, continue=false}
				end;

			{ok, ElapsedUs, local_register, Res} ->
				case register_results(Next, ok, ElapsedUs, 0) of
					ok ->
						State2=State#state {next_op=local_whereis, data=Res, continue=true};
					finish ->
						State2=State#state {next_op=local_whereis, data=Res, continue=false}
				end;

			{ok, ElapsedUs, local_whereis, Res} ->
				case register_results(Next, ok, ElapsedUs, 0) of
					ok ->
						State2=State#state {next_op=local_unregister, data=Res, continue=true};
					finish ->
						State2=State#state {next_op=local_unregister, data=Res, continue=false}
				end;

			{'EXIT', Reason} ->
				%% Operation failed, generate a crash error and terminate.
				case register_results(Next, {error, Reason}, 0, 0) of
					ok ->
						State2=State;
					finish ->
						State2=State#state {continue=false}
				end
		end
	end,
	case needs_shutdown(State2) of
		true ->
			ok;
		false ->
				worker_active_loop(State2)
	end.

register_results(Operation, Result, ElapsedTime, Level) ->
  try de_bench_stats:op_complete(Operation, Result, ElapsedTime)
  catch
    exit:{timeout,Why} -> ?ERROR("stats process timeout in level ~p : ~p ~n", [Level, Why]),
	timer:sleep(1),
	if 
		Level > 5 -> 
			ok;
		Level =< 5 ->
			register_results(Operation, Result, ElapsedTime, Level+1)
	end
  end.

%%
%% Stop a worker process via the supervisor and terminate the app
%% if there are no workers remaining
%%
%% WARNING: Must run from a process other than the worker!
%%

stop_worker(SupChild) ->
    ok = de_bench_sup:stop_child(SupChild),
    case de_bench_sup:workers() of
        [] ->
            %% No more workers -- stop the system
            de_bench_app:stop();
        _ ->
            ok
    end.

%%
%% Expand operations list into tuple suitable for weighted, random draw
%%
ops_tuple() ->
    F =
        fun({OpTag, Count}) ->
                lists:duplicate(Count, {OpTag, OpTag});
           ({Label, OpTag, Count}) ->
                lists:duplicate(Count, {Label, OpTag})
        end,
    Ops = [F(X) || X <- de_bench_config:get(operations, [])],
    list_to_tuple(lists:flatten(Ops)).


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
    
    
