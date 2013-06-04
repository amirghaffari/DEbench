%% DEbench: A benchmarking suite for distributed Erlang
%% author: Amir Ghaffari
%% @RELEASE project (http://www.release-project.eu/)

-module(de_commands).

-export([run/3, getback_remote_data/2]).

-include("de_bench.hrl").

run(TargetNode, Ope, Data) ->
	case Ope of

		%% spawn a process on remote TargetNode node
		spawning ->
			Packet=de_helper:data_block(),
			Start = now(),
			spawn_link(TargetNode,?MODULE, getback_remote_data, [self(), Packet]),
			receive
				{spawn_result, true, Packet} ->
					ElapsedUs = timer:now_diff(now(), Start),
					{ok, ElapsedUs, true};
				{spawn_result, false, Packet} ->
					ElapsedUs = timer:now_diff(now(), Start),
					{error, ElapsedUs, spawn_size_error}	
			after timer:seconds(60) ->
					ElapsedUs = timer:now_diff(now(), Start),
				    {error, ElapsedUs, spawn_timeout_error}
			end;

		%% register a name globally 
		global_register ->
			ProcessName=de_helper:get_timestamp(),
			Start = now(),
			global:register_name(ProcessName, self()),
			ElapsedUs = timer:now_diff(now(), Start),
			{ok, ElapsedUs, global_register, ProcessName};

		%% unregister a name globally
		global_unregister ->
			Start = now(),
			global:unregister_name(Data),
			ElapsedUs = timer:now_diff(now(), Start),
			{ok, ElapsedUs, global_unregister};

		%% queries a name globally
		global_whereis ->
			Start = now(),
			Pid=global:whereis_name(Data),
			ElapsedUs = timer:now_diff(now(), Start),
			case is_pid(Pid) of
			true ->
				{ok, ElapsedUs, global_whereis, Data};
			false ->
				{error, ElapsedUs, whereis_error, Data}
			end;

		%% makes a RPC call on TargetNode node
		remote_call ->
			Packet=de_helper:data_block(),
			Start = now(),
            case rpc:call(TargetNode, ?MODULE, getback_remote_data, [rpc, Packet], timer:seconds(60)) of
                {rpc_result, true, Result} ->
					ElapsedUs = timer:now_diff(now(), Start),
					{ok, ElapsedUs, Result};
                {rpc_result, false, _Result} ->
					ElapsedUs = timer:now_diff(now(), Start),
					{error, ElapsedUs, rpc_size_problem};
                {badrpc, Reason} ->
					ElapsedUs = timer:now_diff(now(), Start),
                    {error, ElapsedUs, Reason}
            end;

		%% makes a RPC call on TargetNode node
		local_register ->
			ProcessName=list_to_atom(de_helper:get_timestamp()),
			Start = now(),
			register(ProcessName, self()),
			ElapsedUs = timer:now_diff(now(), Start),
			{ok, ElapsedUs, local_register, ProcessName};

		%% queries a name locally
		local_whereis ->
			Start = now(),
			Pid=whereis(Data),
			ElapsedUs = timer:now_diff(now(), Start),
			case is_pid(Pid) of
			true ->
				{ok, ElapsedUs, local_whereis, Data};
			false ->
				{error, ElapsedUs, local_whereis_error, Data}
			end;

		%% unregister a name locally
		local_unregister ->
			Start = now(),
			unregister(Data),
			ElapsedUs = timer:now_diff(now(), Start),
			{ok, ElapsedUs, local_unregister};

		_ ->
			{error, 0, no_operation}
	end.

getback_remote_data(rpc, Packet) ->
	Size_is=byte_size(Packet),
	Size_shouldbe=de_bench_config:get(packet_size, 1000),
	Delay_remote_function=de_bench_config:get(delay_remote_function, 0),
	Equal=Size_is=:=Size_shouldbe,
	case Equal of
		true -> 
			de_helper:sleep_microsecond(Delay_remote_function),
			{rpc_result, true, Packet};
		false -> {rpc_result, false, Packet}
	end;

getback_remote_data(Sender, Packet) when is_pid(Sender) ->
	Size_is=byte_size(Packet),
	Size_shouldbe=de_bench_config:get(packet_size, 1000),
	Delay_remote_function=de_bench_config:get(delay_remote_function, 0),
	Equal=Size_is=:=Size_shouldbe,
	case Equal of
		true -> 
			de_helper:sleep_microsecond(Delay_remote_function),
			Sender ! {spawn_result, true, Packet};
		false -> Sender ! {spawn_result, false, Packet}
	end.















