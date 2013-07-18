%% DEbench: A benchmarking suite for distributed Erlang
%% author: Amir Ghaffari
%% @RELEASE project (http://www.release-project.eu/)

-module(de_helper).

-export([ping_nodes/2, getRandomm/1, id/0, get_timestamp/0, file_exist/1, data_block/0, sleep_microsecond/1, get_S_Groups/0 ]).

-include("de_bench.hrl").

getRandomm(Max) ->              
        {_A, B, C} = erlang:now(),           
        random:seed(C, B, C),
        random:uniform(Max).


%% Ping [Nodes]
ping_nodes([],Pangs) ->
	Pangs;
ping_nodes([Node|RemNodes],Pangs) ->
	Ping = net_adm:ping(Node),
	case Ping of
		pong ->
			ping_nodes(RemNodes,Pangs);
		pang ->
			ping_nodes(RemNodes,[Node|Pangs])
	end.

%%=============================================================

%%
%% Construct a string suitable for use as a unique ID 
%%
id() ->
    {{Y, M, D}, {H, Min, S}} = calendar:local_time(),
    ?FMT("~w~2..0w~2..0w_~2..0w~2..0w~2..0w", [Y, M, D, H, Min, S]).

get_timestamp() ->
    {{Y, M, D}, {H, Min, S}} = calendar:local_time(),
    {_,_,Micro} = erlang:now(),
    R=getRandomm(10000000),
    ?FMT("~w~2..0w~2..0w~2..0w~2..0w~2..0w2~w~w", [Y, M, D, H, Min, S, Micro,R]).

file_exist(Filename) ->
    case file:read_file_info(Filename) of
        {ok, _}         -> io:format("~s is found~n", [Filename]);
        {error, enoent} -> io:format("~s is missing~n", [Filename]);
        {error, Reason} -> io:format("~s is ~s~n", [Filename, Reason])
    end.

data_block() ->
Size=de_bench_config:get(packet_size, 1000),
crypto:rand_bytes(Size).

sleep_microsecond(SleepTime) ->
	sleep_microsecond(now(), SleepTime).
 
sleep_microsecond(Start, SleepTime) ->
	ElapsedTime = timer:now_diff(now(), Start),
	if  
		ElapsedTime > SleepTime ->
			ok;
		true ->
			sleep_microsecond(Start, SleepTime)
	end.

get_S_Groups() ->
	case module_exists(s_group) of
		true -> %% Erlang OTP supports SDErlang
			try s_group:own_s_groups() of
				List_of_S_groups -> s_Groups_Names(List_of_S_groups,[]) 
			catch
				_-> List_of_S_groups=[],  
				s_Groups_Names(List_of_S_groups,[])
			end;
		false -> %% Erlang OTP supports SDErlang
			[]
	end.


s_Groups_Names([],List) ->
	List;
s_Groups_Names([{SGroupName, _Nodes}|Tail],List) ->
	s_Groups_Names(Tail, [SGroupName|List]).

module_exists(Module) ->
    case is_atom(Module) of
        true ->
            try Module:module_info() of
                _InfoList ->
                    true
            catch
                _:_ ->
                    false
            end;
 
        false ->
            false
    end.
