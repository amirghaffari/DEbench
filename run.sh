#!/bin/bash 

# author: Amir Ghaffari
# @RELEASE project (http://www.release-project.eu/)

# This bash script runs DEbench on a cluster
# To run on a cluster, you need to specify the cluster information (like number of nodes and nodes name) in files: run.sh and experiment.sh
# How to run:  ./run.sh

SDErlang=true
group_size=2;

for experiment in  1 #  2 3
do     

	spawn_percentage=100
	remote_call_percentage=100

	global_register_percentage=1
	global_unregister_percentage=1
	global_whereis_percentage=1

	local_register_percentage=0
	local_unregister_percentage=0
	local_whereis_percentage=0
	

	Base_directory=`pwd`;

	Base_Result_directory="${Base_directory}/results/heriot_watt";
	Max_node=32;

	let Exceed_Max=$Max_node+1

	Source_direcory="${Base_directory}";
	Result_directory="${Base_Result_directory}/spawn_${spawn_percentage}_rpc_${remote_call_percentage}_global_${global_register_percentage}_local_${local_register_percentage}_expriment_${experiment}";

	if [ ! -d "$Base_directory" ]; then
		echo "Base Directory does not exist: $Base_directory"
		exit;
	fi

	if [ ! -d "$Source_direcory" ]; then
		echo "Source Directory does not exist: $Source_direcory"
		exit;
	fi


	if [ ! -d "$Base_Result_directory" ]; then
		mkdir -p $Base_Result_directory;
	fi

	if [ ! -d "$Result_directory" ]; then
		mkdir $Result_directory;
	fi


	cd $Source_direcory

	if [ -z "$String_Do_not_send_command" ]; then 
		String_Do_not_send_command="empty"
	fi

	for Number_of_Erlang_Nodes in 5 # 10 20 30 40 50 60 70 80 90 100
	do     
		let Number_of_VMs_per_Nodes=$Number_of_Erlang_Nodes/$Exceed_Max;
		if [ $Number_of_VMs_per_Nodes -eq 0 ]
		then
			Number_of_VMs_per_Nodes=1;
		else
			let Number_of_VMs_per_Nodes=$Number_of_VMs_per_Nodes+1
		fi

		if [ $Number_of_Erlang_Nodes -lt $Exceed_Max ]
		then
			Total_nodes=$Number_of_Erlang_Nodes
		else
			Total_nodes=$Max_node
		fi

		./experiment.sh  $Total_nodes $Number_of_VMs_per_Nodes $spawn_percentage $remote_call_percentage $global_register_percentage  $local_register_percentage $Result_directory $group_size $SDErlang; 

	done
done




