%% DE-Bench: A benchmarking suite for distributed Erlang
%% This module runs a gen-server process to handle incoming requests

%% Author: Amir Ghaffari <Amir.Ghaffari@glasgow.ac.uk>
%% RELEASE project (http://www.release-project.eu/)

-module(de_bench_server).

-behaviour(gen_server).


%% API
-export([start_link/0, service/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("de_bench.hrl").

-record(state, {packet_size, delay_remote_function}).  


%% ====================================================================
%% API
%% ====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

service(TargetNode, Packet) ->
    gen_server:call({?MODULE, TargetNode}, {service, Packet}).

%% ====================================================================
%% gen_server callbacks
%% ====================================================================

init([]) ->
	io:format("gen_server process starts at ~p ~n",[erlang:time()]),
	Packet_size= de_bench_config:get(packet_size, 1000),
	Delay_remote_function= de_bench_config:get(delay_remote_function, 0),
    {ok, #state{ packet_size = Packet_size,
                 delay_remote_function = Delay_remote_function
                 }}.

handle_call({service, Packet}, _From, State) ->
	Size_is=byte_size(Packet),
	Size_shouldbe=State#state.packet_size,
	Delay_remote_function=State#state.delay_remote_function,
	Equal=Size_is=:=Size_shouldbe,
	case Equal of
		true -> 
			de_helper:sleep_microsecond(Delay_remote_function),
			{reply, {true, Packet}, State};
		false -> {reply, {false, Packet}, State}
	end.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


