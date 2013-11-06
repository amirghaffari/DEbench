#!/bin/bash 

# Author: Amir Ghaffari <Amir.Ghaffari@glasgow.ac.uk>
# RELEASE project (http://www.release-project.eu/)


BwlfCluster=true # Specifies whether use Heriot-Watt University's beowulf cluster or use Uppsala University's kalkyl cluster
SDErlang=false # Specifies whether use SD Erlang or not
group_size=10; # Specifies the size of s_groups in SD Erlang
Duration=5; # benchmark's duration in minute
packet_size=10 # size of data that is send and received in spawn and rpc commands
delay_remote_function=1 # specifies how long remote function takes in microsecond. This is the fucntion that is called by spawn or rpc remotely
Prefix='normal'; # a prefix for grouping the results

# specifies the ratio of each command
spawn_percentage=1000 # ratio of spawn
remote_call_percentage=1000 # ratio of rpc

global_register_percentage=10 # ratio of globally name registration
global_unregister_percentage=10 # ratio of globally name unregistration
global_whereis_percentage=10 # ratio of globally name query

local_register_percentage=10 # ratio of local name registration
local_unregister_percentage=10 # ratio of local name unregistration
local_whereis_percentage=10 # ratio of local name query

gen_server_call_percentage=10; # ratio of gen_server call
fsm_server_call_percentage=10; # ratio of fsm server call

# end of specifying the ratio of each command

# sleep time definition is included
source constant.sh

# Specifies how many experiment is needed
for experiment in  1
do

	Base_directory=`pwd`;
	if $BwlfCluster ; then
		Base_Result_directory="${Base_directory}/results/heriot_watt";
		Max_node=32; # available node at Heriot-Watt's beowulf cluster  
	else
		Base_Result_directory="${Base_directory}/results/uppsala";
		Max_node=160; # available node at Uppsala's kalkyl cluster  
	fi
	let Exceed_Max=$Max_node+1

	Source_direcory="${Base_directory}";
	if $SDErlang ; then
		Result_directory="${Base_Result_directory}/SDErlang/$Prefix/packet_size_${packet_size}_delay_remote_${delay_remote_function}_spawn_${spawn_percentage}_rpc_${remote_call_percentage}_global_${global_register_percentage}_local_${local_register_percentage}_gen_server_${gen_server_call_percentage}_fsm_${fsm_server_call_percentage}_expriment_${experiment}";
	else
		Result_directory="${Base_Result_directory}/DErlang/$Prefix/packet_size_${packet_size}_delay_remote_${delay_remote_function}_spawn_${spawn_percentage}_rpc_${remote_call_percentage}_global_${global_register_percentage}_local_${local_register_percentage}_gen_server_${gen_server_call_percentage}_fsm_${fsm_server_call_percentage}_expriment_${experiment}";
	fi

	if [ ! -d "$Base_directory" ]; then
		echo "Base Directory does not exist: $Base_directory"
		exit;
	fi

	if [ ! -d "$Result_directory" ]; then
		mkdir -p $Result_directory;
	fi

	cd $Source_direcory
	current_path=`pwd`;
	if [ $current_path = $Source_direcory ]
	then
		# remove files that belong to previous executions
		rm -f slurm*;
		rm -f shell_output*;
		rm -f experiment_*;
	fi

	for Number_of_Erlang_Nodes in 10
	do     
		let Number_of_VMs_per_Nodes=$Number_of_Erlang_Nodes/$Exceed_Max;

		let temp=$Exceed_Max*$Number_of_VMs_per_Nodes;

		if [ $Number_of_VMs_per_Nodes -eq 0 ]
		then
			Number_of_VMs_per_Nodes=1;
		else
			if [ $Number_of_Erlang_Nodes -ne $temp ] ; then
				let Number_of_VMs_per_Nodes=$Number_of_VMs_per_Nodes+1
			fi
		fi

		if [ $Number_of_Erlang_Nodes -lt $Exceed_Max ]
		then
			Total_nodes=$Number_of_Erlang_Nodes
		else
			Total_nodes=$Max_node
		fi

		if $BwlfCluster ; then
			chmod 755 experiment.sh
			./experiment.sh $Total_nodes $Number_of_VMs_per_Nodes $spawn_percentage $remote_call_percentage $global_register_percentage  $local_register_percentage $gen_server_call_percentage $fsm_server_call_percentage $Result_directory $BwlfCluster $group_size $SDErlang $Duration $packet_size $delay_remote_function; 
		else
			# calculate how long this benchmark will take
			let Total_number_of_Erlang_Nodes=$Total_nodes*$Number_of_VMs_per_Nodes
			let Duration_sec=$Duration*60;
			Sleep_time_after_ping=$Total_number_of_Erlang_Nodes;
			Sleep_time_before_ping=$Total_number_of_Erlang_Nodes;
			Sleep_time_after_bench_finished=$Total_number_of_Erlang_Nodes;
			let Sleep_for_copying_file=$Total_number_of_Erlang_Nodes*2;
			let aggregating_time=$time_for_aggregating_each_node*$Total_number_of_Erlang_Nodes;
			let profiling_time=$time_for_profiling_each_node*$Total_nodes;
			
			let Total_benchmark_time_seconds=$Duration_sec+$Sleep_time_after_ping+$Sleep_time_before_ping+$Sleep_time_after_bench_finished+$Sleep_for_copying_file+$Sleep_after_copying_file+$Sleep_for_copying_file+$Sleep_after_copying_file+$Sleep_addition_after_benchmark+$aggregating_time+$profiling_time
			convertsecs() {
			 ((h=${1}/3600))
			 ((m=(${1}%3600)/60))
			 ((s=${1}%60))
			 printf "%02d:%02d:%02d\n" $h $m $s
			}
			
			Bench_time=$(convertsecs $Total_benchmark_time_seconds)
			########## end of time calculation
			let Total_cores=Total_nodes*8;
			String_format_sbatch="SBATCH -p node -N ${Total_nodes} -n ${Total_cores}";
			sed "s/SBATCH -p node -N 0 -n 0/$String_format_sbatch/g" experiment.sh>experiment_${Total_nodes}_vms_${Number_of_VMs_per_Nodes}_spawn_${spawn_percentage}_global_register_${global_register_percentage}_local_register_${local_register_percentage}_expriment_${experiment};
			sed -i "s/00:00:00/$Bench_time/g" experiment_${Total_nodes}_vms_${Number_of_VMs_per_Nodes}_spawn_${spawn_percentage}_global_register_${global_register_percentage}_local_register_${local_register_percentage}_expriment_${experiment};
			chmod 755 experiment_${Total_nodes}_vms_${Number_of_VMs_per_Nodes}_spawn_${spawn_percentage}_global_register_${global_register_percentage}_local_register_${local_register_percentage}_expriment_${experiment};
			sbatch experiment_${Total_nodes}_vms_${Number_of_VMs_per_Nodes}_spawn_${spawn_percentage}_global_register_${global_register_percentage}_local_register_${local_register_percentage}_expriment_${experiment} $Total_nodes $Number_of_VMs_per_Nodes $spawn_percentage $remote_call_percentage $global_register_percentage  $local_register_percentage $gen_server_call_percentage $fsm_server_call_percentage $Result_directory $BwlfCluster $group_size $SDErlang $Duration $packet_size $delay_remote_function; 
		fi

	done
done



