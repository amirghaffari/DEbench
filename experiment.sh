#!/bin/bash
#SBATCH -A p2012172
#SBATCH -p node -N 0 -n 0
#SBATCH -t 00:00:00 

# author: Amir Ghaffari
# @RELEASE project (http://www.release-project.eu/)

# sleep time definition
source constant.sh

Start_Node=1;

Total_Nodes=$1; # (number of Erlang VMs per each node) * (number of nodes)
Number_of_VMs=$2
spawn_percentage=$3
remote_call_percentage=$4
global_register_percentage=$5
local_register_percentage=$6
Base_Result_directory=$7;
BwlfCluster=$8
group_size=$9
SDErlang=${10}
Duration=${11} # benchmark's duration

Base_directory=`pwd`;

# you need to specify paths for Erlang, SD Erlang, and R to create graph

if $BwlfCluster ; then
	if $SDErlang ; then
		Erlang_path="/u1/pg/ag275/Desktop/sderlang/sderlang/bin";
	else
		Erlang_path="/u1/pg/ag275/erlang/bin";
	fi
	R_path="/u1/pg/ag275/R/R-2.15.1/bin";
	SNIC_TMP="/scratch"
	Killing_nodes=$Total_Nodes
else
	if $SDErlang ; then
		Erlang_path="/bubo/home/h8/ag275/sderlang/bin";
	else
		Erlang_path="/bubo/home/h8/ag275/erlang/bin";
	fi
	R_path="/bubo/home/h8/ag275/R/bin";
	Killing_nodes=0;
fi

VM_Name="node";
Source_direcory=${Base_directory};
Original_Config_File="template_bench.config";
Original_SDErlang_Config_File="template_sderlang.config";
SDErlang_Config_File="sderlang.config";


if [ ! -d "$Base_directory" ]; then
	echo "Base Directory does not exist: $Base_directory"
	exit;
fi

if [ ! -d "$Base_Result_directory" ]; then
	mkdir -p $Base_Result_directory;
fi

cd $Source_direcory;

if $BwlfCluster ; then
	let To_Node=$Start_Node+$Total_Nodes-1;
	Node_Counter=0;
	for index in `seq $Start_Node $To_Node`; do 
			let Node_Counter=$Node_Counter+1;
			if [ "$index" -lt 10 ]
			then
				Hostnames[$Node_Counter]="bwlf0${index}.macs.hw.ac.uk";
				tempip=`ssh -q ${Hostnames[$Node_Counter]} "hostname -i;"`;
				IPaddresses[$Node_Counter]=$tempip;
			else
				Hostnames[$Node_Counter]="bwlf${index}.macs.hw.ac.uk";
				tempip=`ssh -q ${Hostnames[$Node_Counter]} "hostname -i;"`;
				IPaddresses[$Node_Counter]=$tempip;
				echo "more than 10 ($index) ${Hostnames[$Node_Counter]}  ${IPaddresses[$Node_Counter]} " 
			fi
	done
else
	for index in `seq 1 $Total_Nodes`; do 
		let zero_index=$index-1;
		tempip=`srun -r $zero_index  -N 1 -n 1 bash -c "hostname -i"`; 
		while [ -z "$tempip" ]; do
			sleep 1;
			tempip=`srun -r $zero_index  -N 1 -n 1 bash -c "hostname -i"`;
		done
		temphostname=`srun -r  $zero_index -N 1 -n 1 bash -c hostname`;
		while [ -z "$temphostname" ]; do
			sleep 1;
			temphostname=`srun -r  $zero_index -N 1 -n 1 bash -c hostname`;
		done
		IPaddresses[$index]=$tempip;
		Hostnames[$index]=$temphostname;
	done
fi


Number_of_Nodes=$Total_Nodes;
let Total_number_of_Erlang_Nodes=$Number_of_Nodes*$Number_of_VMs;

Config_file="bench_nodes_${Number_of_Nodes}_VMs_${Number_of_VMs}.config";
Result_directory="${Base_Result_directory}/Nodes_${Number_of_Nodes}_VMs_${Number_of_VMs}";

if [ ! -d "$Result_directory" ]; then
	mkdir -p $Result_directory;
else
	cd $Result_directory;
	current_path=`pwd`;
	if [ $current_path = $Result_directory ]
	then
		rm -rf *;
	fi
fi

for index in `seq 1 $Number_of_VMs`; do 
	VMs[$index]="${VM_Name}${index}@"
done

Output_file_name="${Result_directory}/shell_output_DE_benchmark_${Number_of_Nodes}_${Number_of_VMs}";
echo "Start at time :">$Output_file_name;
date +'%T'>>$Output_file_name;

for index in `seq 1 $Number_of_Nodes`; do 
	echo "IP is ${IPaddresses[$index]} and name is ${Hostnames[$index]} for index $index">>$Output_file_name;
done
echo "========================================================">>$Output_file_name;

Qoute_mark="'";
Comma_mark=",";
String_format_addresses="";

for index in `seq 1 $Number_of_Nodes`; do
	for counter in `seq 1 $Number_of_VMs`; do
		VMname=${VMs[$counter]}
		if [ $index -eq 1 -a $counter -eq 1 ]
		then
			String_format_addresses=${String_format_addresses}${Qoute_mark}${VMname}${Hostnames[$index]}${Qoute_mark}
		else
			String_format_addresses=${String_format_addresses}${Comma_mark}${Qoute_mark}${VMname}${Hostnames[$index]}${Qoute_mark}
		fi
	done
done

let Duration_sec=$Duration*60;
let Report_interval_seconds=$Duration_sec/10;
Sleep_time_after_ping=$Total_number_of_Erlang_Nodes;
let Sleep_for_copying_file=$Number_of_VMs*2;


echo "Name of VM nodes are: $String_format_addresses">>$Output_file_name;
echo "======= Duration is $Duration_sec seconds  and Sleep_time_after_ping is $Sleep_time_after_ping seconds =========">>$Output_file_name;

if $SDErlang ; then
	let NumberOfGroup=$Total_number_of_Erlang_Nodes/$group_size+1;
	index_total_nodes=0;
	SGroups="";
	for group_num in `seq 1 $NumberOfGroup`; do
		if [ $group_num -lt $NumberOfGroup ]
		then
			Groupsize=$group_size
		else
			let temp1=$group_num-1;
			let temp1=$group_size*$temp1
			let Groupsize=$Total_number_of_Erlang_Nodes-$temp1
		fi
		String_temp="";
		for counter in `seq 1 $Groupsize`; do
			let index_total_nodes=$index_total_nodes+1;
			let temp1=${index_total_nodes}/$Number_of_VMs;
			let temp2=$temp1*$Number_of_VMs;
			let VMIndex=${index_total_nodes}-$temp2;
			if [ $VMIndex -eq 0 ] 
			then
				VMIndex=$Number_of_VMs
				let PhysicalNodeIndex=$temp1;
			else
				let PhysicalNodeIndex=$temp1+1;
			fi
			NodeName=${VMs[$VMIndex]}${Hostnames[$PhysicalNodeIndex]}
			
			if [ $counter -eq 1 ]
			then
				String_temp=${String_temp}${Qoute_mark}${NodeName}${Qoute_mark}
			else
				String_temp=${String_temp}${Comma_mark}${Qoute_mark}${NodeName}${Qoute_mark}
			fi

		done
		if [ $group_num -eq 1 ]
		then
			SGroups="{group${group_num}, normal, [$String_temp]}"
		else
			SGroups="$SGroups, {group${group_num}, normal, [$String_temp]}"
		fi
	done
fi

for index in `seq 1 $Killing_nodes`; do 
ssh -q ${IPaddresses[$index]} "
echo '========================= killing (index=$index) ==================';
pwd;
hostname -i;
hostname;
date +'%T';
echo 'befor kill=====';
top -b -n 1 | grep beam.smp;
pkill beam.smp;
pkill epmd;
echo 'after kill=====';
top -b -n 1 | grep beam.smp;
echo 'time:';
date +'%T';
echo '===========================================';
";
done

echo "==========================================After killing VMs">>$Output_file_name;
date +'%T'>>$Output_file_name;

for index in `seq 1 $Number_of_Nodes`; do 
(ssh -q ${IPaddresses[$index]} "
echo '===========================================';
PATH=$Erlang_path:$PATH;
export PATH;
cd $SNIC_TMP;
rm -rf de_bench/
mkdir de_bench;
cd de_bench;
pkill beam.smp;
echo 'Running Erlang VM on hostname and path at time:';
pwd;
hostname -i;
hostname;
date +'%T';
for counter in {1..$Number_of_VMs}
do
	cd $SNIC_TMP;
	cd de_bench;
	echo '===============';
	VMname=\"${VM_Name}\${counter}@\";
	VMname_Dir=\"${VM_Name}\${counter}\";
	mkdir \${VMname_Dir};
	cd \${VMname_Dir};
	#cp -rf ${Source_direcory}/* .;
	cp -rf ${Source_direcory}/deps ${Source_direcory}/ebin ${Source_direcory}/priv ${Source_direcory}/${Original_Config_File} ${Source_direcory}/${Original_SDErlang_Config_File} .;
	sed \"s/----------/$SGroups/g\" $Original_SDErlang_Config_File>$SDErlang_Config_File;
	sed \"s/Here_put_VMs_names/$String_format_addresses/g\" $Original_Config_File>$Config_file;
	sed -i \"s/sleep_time_after_ping_here/$Sleep_time_after_ping/g\" $Config_file;
	sed -i \"s/sleep_time_before_ping_here/$Sleep_time_after_ping/g\" $Config_file;
	sed -i \"s/delay_after_bench_finished_here/$Sleep_time_after_ping/g\" $Config_file;
	
	sed -i \"s/minutes/$Duration/g\" $Config_file;
	sed -i \"s/report_interval_seconds/$Report_interval_seconds/g\" $Config_file;
	sed -i \"s/local_node_name/\${VMname}${Hostnames[$index]}/g\" $Config_file;

	sed -i \"s/spawn_percentage/$spawn_percentage/g\" $Config_file;
	sed -i \"s/remote_call_percentage/$remote_call_percentage/g\" $Config_file;

	sed -i \"s/global_register_percentage/$global_register_percentage/g\" $Config_file;
	sed -i \"s/global_unregister_percentage/$global_register_percentage/g\" $Config_file;
	sed -i \"s/global_whereis_percentage/$global_register_percentage/g\" $Config_file;

	sed -i \"s/local_register_percentage/$local_register_percentage/g\" $Config_file;
	sed -i \"s/local_unregister_percentage/$local_register_percentage/g\" $Config_file;
	sed -i \"s/local_whereis_percentage/$local_register_percentage/g\" $Config_file;

	#zip -r Source_Host_${index}_VM_\${counter}.zip *;
	#mv  Source_Host_${index}_VM_\${counter}.zip $Result_directory;
	echo \"Benchmark runs on Erlang node (\${VMname}${Hostnames[$index]}) on node ${Hostnames[$index]} at time:\";
	date +'%T';
done

top -b -n 1 | grep beam.smp;

date +'%T';
echo '===========================================';
")&
sleep $Sleep_for_copying_file;
done
sleep $Sleep_after_copying_file;
echo "=================After coping Benchmarks">>$Output_file_name;
date +'%T'>>$Output_file_name;

for index in `seq 1 $Number_of_Nodes`; do 
(ssh -q ${IPaddresses[$index]} "
echo '===========================================';
PATH=$Erlang_path:$R_path:$PATH;
export PATH;
for counter in {1..$Number_of_VMs}
do
	cd $SNIC_TMP;
	cd de_bench;
	echo '===============';
	VMname_Dir=\"${VM_Name}\${counter}\";
	VMname=\"${VM_Name}\${counter}@\";
	cd \${VMname_Dir};
	echo 'before run erlang:'
	date +'%T';
	echo 'current path:'>${Output_file_name}_nodes_${index}_VMs_${Number_of_VMs}_\${VMname_Dir};
	pwd>>${Output_file_name}_nodes_${index}_VMs_${Number_of_VMs}_\${VMname_Dir};
	echo 'On host:'>>${Output_file_name}_nodes_${index}_VMs_${Number_of_VMs}_\${VMname_Dir};
	hostname>>${Output_file_name}_nodes_${index}_VMs_${Number_of_VMs}_\${VMname_Dir};

	if $SDErlang ; then
		(erl -noshell -name \${VMname}${Hostnames[$index]} -config $SDErlang_Config_File -run de_bench main $Config_file -s init stop -pa ebin -pa deps/*/ebin>>${Output_file_name}_nodes_${index}_VMs_${Number_of_VMs}_\${VMname_Dir})&
	else
		(erl -noshell -name \${VMname}${Hostnames[$index]} -run de_bench main $Config_file -s init stop -pa ebin -pa deps/*/ebin>>${Output_file_name}_nodes_${index}_VMs_${Number_of_VMs}_\${VMname_Dir})&
	fi

	echo 'after run erlang:';
	date +'%T';
	echo \"Benchmark runs on Erlang node (\${VMname}${Hostnames[$index]}) on node ${Hostnames[$index]} at time:\";
	date +'%T';
done
top -b -n 1 | grep beam.smp;

date +'%T';
echo '===========================================';
")&
done
date +'%T'>>$Output_file_name;
Sleep_time_before_ping=$Sleep_time_after_ping;
Delay_after_bench_finished=$Sleep_time_after_ping;
let Total_sleep=$Duration_sec+$Sleep_time_after_ping+$Delay_after_bench_finished+$Sleep_time_before_ping+$Sleep_addition_after_benchmark;
echo "=============== After starting Benchmarks so sleep for ( $Total_sleep ) seconds";
echo "=============== After starting Benchmarks so sleep for ( $Total_sleep ) seconds">>$Output_file_name;
date +'%T'>>$Output_file_name;
echo "========================================================">>$Output_file_name;
sleep $Total_sleep;
echo "after sleep ===========================">>$Output_file_name;
date +'%T'>>$Output_file_name;

	
for index in `seq 1 $Number_of_Nodes`; do 
(ssh -q ${IPaddresses[$index]} "
PATH=$Erlang_path:$R_path:$PATH;
export PATH;
echo '===========================================';
for counter in {1..$Number_of_VMs}
do
	cd $SNIC_TMP;
	cd de_bench;
	echo \"=======  index = $index and counter = \${counter}  ========\";
	VMname_Dir=\"${VM_Name}\${counter}\";
	cd \${VMname_Dir};
	#priv/summary.r -i tests/current;
	cp $SDErlang_Config_File tests/current;
	find -name 'erl_crash.dump' -exec cp {} tests/current  \;
	cd tests/current;
	echo 'list of files before zip ============================================================='>>${Output_file_name}_nodes_${index}_VMs_${Number_of_VMs}_\${VMname_Dir};
	ls  *.csv *.config *.png>>${Output_file_name}_nodes_${index}_VMs_${Number_of_VMs}_\${VMname_Dir};
	zip -r Hosts_${Number_of_Nodes}_VMs_${Number_of_VMs}_Host_${index}_VM_\${counter}.zip *.csv *.config *.png *.dump;
	mv  Hosts_${Number_of_Nodes}_VMs_${Number_of_VMs}_Host_${index}_VM_\${counter}.zip $Result_directory;
done
")&
sleep $Sleep_for_copying_file;
done
sleep $Sleep_after_copying_file;

# aggregating the collected results of all participating nodes
cd $Result_directory;
zip -r All.zip *.zip
mkdir all;
mv All.zip all
cd $Source_direcory;
rm csv_tool.class
javac csv_tool.java
java csv_tool "$Result_directory/all/aggregated" "$Result_directory/all";
cd "$Result_directory/all"
rm -fr All
rm All.zip
cd "$Result_directory/all/aggregated"
unzip "$Result_directory/all/aggregated/All.zip"
rm All.zip
PATH=$Erlang_path:$R_path:$PATH;
export PATH;
cd $Source_direcory;
priv/summary.r -i "$Result_directory/all/aggregated"
echo "Benchmark ended at time">>$Output_file_name;
date +'%T'>>$Output_file_name;

