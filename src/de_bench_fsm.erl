%% DE-Bench: A benchmarking suite for distributed Erlang
%% This module runs a FSM process to handle incoming requests

%% Author: Amir Ghaffari <Amir.Ghaffari@glasgow.ac.uk>
%% RELEASE project (http://www.release-project.eu/)

-module(de_bench_fsm).

-behaviour(gen_fsm).


%% API
-export([start_link/0, service/2]).

%% gen_fsm callbacks
-export([init/1, state1/3, handle_info/3, terminate/3, code_change/4, handle_event/3, handle_sync_event/4]).

-include("de_bench.hrl").

-record(state, {packet_size, delay_remote_function}).  


%% ====================================================================
%% API
%% ====================================================================

start_link() ->
	gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

service(TargetNode, Packet) ->
	gen_fsm:sync_send_event({?MODULE, TargetNode},  Packet).
%% ====================================================================
%% gen_fsm callbacks
%% ====================================================================

init([]) ->
	io:format("fsm process starts at ~p ~n",[erlang:time()]),
	Packet_size= de_bench_config:get(packet_size, 1000),
	Delay_remote_function= de_bench_config:get(delay_remote_function, 0),
	{ok,state1, #state{	packet_size = Packet_size,
							delay_remote_function = Delay_remote_function
				}
	}.

handle_event(_Event, _StateName, _StateData) -> 
ok.

handle_sync_event(_Event, _From, _StateName, _StateData) -> 
ok.

state1(Packet, _From, State) ->
	Size_is=byte_size(Packet),
	Size_shouldbe=State#state.packet_size,
	Delay_remote_function=State#state.delay_remote_function,
	Equal=Size_is=:=Size_shouldbe,
	case Equal of
		true -> 
			de_helper:sleep_microsecond(Delay_remote_function),
			{reply,{true, Packet},state1,State};
		false -> {reply,{error, Packet},state1,State}
	end.

terminate(_Reason, _StateName, _StateData) ->
	ok.

handle_info(_Info, StateName, StateData) -> 
	{next_state,StateName,StateData}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
	{ok, StateName, StateData}.

