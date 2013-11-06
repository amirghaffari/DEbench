#!/bin/bash -l
#SBATCH -A p2012172
#SBATCH -p node -N 0 -n 0
#SBATCH -t 00:00:00 

# Author: Amir Ghaffari <Amir.Ghaffari@glasgow.ac.uk>
# RELEASE project (http://www.release-project.eu/)

# sleep time definition
source constant.sh

Start_Node=1;

Total_Nodes=$1; # (number of Erlang VMs per each node) * (number of nodes)
Number_of_VMs=$2
spawn_percentage=$3
remote_call_percentage=$4
global_register_percentage=$5
local_register_percentage=$6
gen_server_call_percentage=$7
fsm_server_call_percentage=$8 
Base_Result_directory=$9
BwlfCluster=${10}
group_size=${11}
SDErlang=${12}
Duration=${13} # benchmark's duration
packet_size=${14}
delay_remote_function=${15}

Base_directory=`pwd`;

# you need to specify paths for Erlang, SD Erlang, and R to create graph

if $BwlfCluster ; then
	if $SDErlang ; then
		Erlang_path="/home/ag275/sderlang/bin";
	else
		Erlang_path="/home/ag275/erlang/bin";
	fi
	R_path="/home/ag275/R/bin";
	SNIC_TMP="/scratch"
	Killing_nodes=$Total_Nodes
else
	if $SDErlang ; then
		Erlang_path="/home/ag275/sderlang/bin";
	else
		Erlang_path="/home/ag275/erlang/bin";
	fi
	R_path="/home/ag275/R/bin";
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
			Result_netstat=`ssh ${Hostnames[$Node_Counter]} "netstat -s"`;
			before_sent_packets[$Node_Counter]=$(echo "$Result_netstat" | grep 'segments send out'| awk  '{print $1}'| head -1)
			before_received_packets[$Node_Counter]=$(echo "$Result_netstat" | grep 'segments received'| awk  '{print $1}'| head -1)
			before_retransmission_packets[$Node_Counter]=$(echo "$Result_netstat" | grep 'retransmited'| awk  '{print $1}')
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
		Result_netstat=`srun -r $zero_index  -N 1 -n 1 bash -c "netstat -s"`;
		before_sent_packets[$index]=$(echo "$Result_netstat" | grep 'segments send out'| awk  '{print $1}'| head -1)
		before_received_packets[$index]=$(echo "$Result_netstat" | grep 'segments received'| awk  '{print $1}'| head -1)
		before_retransmission_packets[$index]=$(echo "$Result_netstat" | grep 'retransmited'| awk  '{print $1}')
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

Output_file_name="${Result_directory}/shell_output_nodes_${Number_of_Nodes}_vms_${Number_of_VMs}";
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
echo "======= packet_size=${packet_size}     delay_remote_function=${delay_remote_function}  =========">>$Output_file_name;

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
kill $(pgrep beam.smp);
echo 'after kill=====';
top -b -n 1 | grep beam.smp;
echo 'time:';
date +'%T';
pkill -u ag275
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
	cp -rf ${Source_direcory}/deps ${Source_direcory}/ebin ${Source_direcory}/priv ${Source_direcory}/${Original_Config_File} ${Source_direcory}/${Original_SDErlang_Config_File} .;
	sed \"s/----------/$SGroups/g\" $Original_SDErlang_Config_File>$SDErlang_Config_File;
	sed \"s/Here_put_VMs_names/$String_format_addresses/g\" $Original_Config_File>$Config_file;
	sed -i \"s/sleep_time_after_ping_here/$Sleep_time_after_ping/g\" $Config_file;
	sed -i \"s/sleep_time_before_ping_here/$Sleep_time_after_ping/g\" $Config_file;
	sed -i \"s/delay_after_bench_finished_here/$Sleep_time_after_ping/g\" $Config_file;

	sed -i \"s/packet_size_here/$packet_size/g\" $Config_file;
	sed -i \"s/delay_remote_function_here/$delay_remote_function/g\" $Config_file;

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
	
	sed -i \"s/gen_server_call_percentage/$gen_server_call_percentage/g\" $Config_file;
	sed -i \"s/fsm_server_call_percentage/$fsm_server_call_percentage/g\" $Config_file;

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
	echo 'current path:'>${Result_directory}/node_${index}_VM_\${counter}
	
	pwd>>${Result_directory}/node_${index}_VM_\${counter}
	echo 'On host:'>>${Result_directory}/node_${index}_VM_\${counter}
	hostname>>${Result_directory}/node_${index}_VM_\${counter}

	if $SDErlang ; then
		(erl -noshell -name \${VMname}${Hostnames[$index]} -config $SDErlang_Config_File -run de_bench main $Config_file -s init stop -pa ebin -pa deps/*/ebin>>${Result_directory}/node_${index}_VM_\${counter})&
	else
		(erl -noshell -name \${VMname}${Hostnames[$index]} -run de_bench main $Config_file -s init stop -pa ebin -pa deps/*/ebin>>${Result_directory}/node_${index}_VM_\${counter})&
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
let half_of_sleep=$Duration_sec+$Sleep_time_after_ping+$Sleep_time_before_ping;
let half_of_sleep=$half_of_sleep/2;

echo "=============== After starting Benchmarks. Total sleep time is ( $Total_sleep ) seconds">>$Output_file_name;
echo "=============== The first half of sleep for ( $half_of_sleep ) seconds">>$Output_file_name;
date +'%T'>>$Output_file_name;
echo "========================================================">>$Output_file_name;
sleep $half_of_sleep;
echo "after sleep ===========================">>$Output_file_name;
date +'%T'>>$Output_file_name;

for index in `seq 1 $Number_of_Nodes`; do 
(ssh -q ${IPaddresses[$index]} "
cd $SNIC_TMP/de_bench;
for counter in {1..$Number_of_VMs}
do
	VMname_Dir=\"${VM_Name}\${counter}\";
	cd \${VMname_Dir}/tests/current;
	mpstat -P ALL 5 1 > cpu_usage.txt;
	echo '===============================================================' >> cpu_usage.txt;
	top -b -n 1 -u $(whoami) >> cpu_usage.txt;
	free -m > memory_usage.txt;
done
")&
done

START=$(date +%s)
sleep 10;

Sum_cpu_usage=0;
Sum_memory_usage=0;
for index in `seq 1 $Number_of_Nodes`; do 

	cpu_usage=`ssh ${IPaddresses[$index]} " top -b -n 1 " | grep 'beam.smp' | awk '{print $9}' | awk '{sum+=$1} END {print sum}'`
	Total_cpu_usage=`ssh ${IPaddresses[$index]} " top -b -n 1 " | awk '{print $9}' | awk '{sum+=$1} END {print sum}'`

	Sum_cpu_usage=$(awk "BEGIN {print $Sum_cpu_usage+$Total_cpu_usage; exit}")

	memory_usage=`ssh ${IPaddresses[$index]} " top -b -n 1 " | grep 'beam.smp' | awk '{print $10}' | awk '{sum+=$1} END {print sum}'`
	Total_memory_usage=`ssh ${IPaddresses[$index]} " top -b -n 1 " | awk '{print $10}' | awk '{sum+=$1} END {print sum}'`

	Sum_memory_usage=$(awk "BEGIN {print $Sum_memory_usage+$Total_memory_usage; exit}")

done
Sum_cpu_usage=$(awk "BEGIN {print $Sum_cpu_usage/$Number_of_Nodes; exit}")
Sum_memory_usage=$(awk "BEGIN {print $Sum_memory_usage/$Number_of_Nodes; exit}")

echo "======= CPU and Memory usage =====================">>$Output_file_name;
echo "average_cpu_usage_percentage: ${Total_number_of_Erlang_Nodes}:${Sum_cpu_usage}">>$Output_file_name;
echo "average_memory_usage_percentage: ${Total_number_of_Erlang_Nodes}:$Sum_memory_usage">>$Output_file_name;
		
END=$(date +%s)
ElapsedTime=$(( $END - $START ))

let half_of_sleep=$Total_sleep-$half_of_sleep-$ElapsedTime;
echo "=============== After profiling. ElapsedTime over profiling is $ElapsedTime seconds. The second half of sleep for ( $half_of_sleep ) seconds">>$Output_file_name;
date +'%T'>>$Output_file_name;
echo "========================================================">>$Output_file_name;
sleep $half_of_sleep;
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
	echo 'list of files before zip ============================================================='>>${Result_directory}/node_${index}_VM_\${counter};	
	ls  *.csv *.config *.png>>${Result_directory}/node_${index}_VM_\${counter};	
	zip -r Hosts_${Number_of_Nodes}_VMs_${Number_of_VMs}_Host_${index}_VM_\${counter}.zip *.csv *.config *.png *.dump *.txt;
	mv  Hosts_${Number_of_Nodes}_VMs_${Number_of_VMs}_Host_${index}_VM_\${counter}.zip $Result_directory;
done
")&
sleep $Sleep_for_copying_file;
done
sleep $Sleep_after_copying_file;

sum_sent_packets=0;
sum_received_packets=0;
sum_retransmission_packets=0;
for index in `seq 1 $Number_of_Nodes`; do 
	if $BwlfCluster ; then
		Result_netstat=`ssh ${IPaddresses[$index]} "netstat -s"`;
	else
		Result_netstat=`srun -r $zero_index  -N 1 -n 1 bash -c "netstat -s"`;
	fi
	after_sent_packets=$(echo "$Result_netstat" | grep 'segments send out'| awk  '{print $1}'| head -1)
	after_received_packets=$(echo "$Result_netstat" | grep 'segments received'| awk  '{print $1}'| head -1)
	after_retransmission_packets=$(echo "$Result_netstat" | grep 'retransmited'| awk  '{print $1}')
	#########
	temp_after_sent_packets=`echo $after_sent_packets | bc`
	temp_after_received_packets=`echo $after_received_packets | bc`
	temp_after_retransmission_packets=`echo $after_retransmission_packets | bc`
	#########
	temp_before_sent_packets=`echo ${before_sent_packets[$index]} | bc`
	temp_before_received_packets=`echo ${before_received_packets[$index]} | bc`
	temp_before_retransmission_packets=`echo ${before_retransmission_packets[$index]} | bc`
	##############
	let sum_sent_packets=$sum_sent_packets+$temp_after_sent_packets-$temp_before_sent_packets;
	let sum_received_packets=$sum_received_packets+$temp_after_received_packets-$temp_before_received_packets;
	let sum_retransmission_packets=$sum_retransmission_packets+$temp_after_retransmission_packets-$temp_before_retransmission_packets;

done

echo "======= Network usage =====================">>$Output_file_name;
echo "sent_packets: ${Total_number_of_Erlang_Nodes}:${sum_sent_packets}">>$Output_file_name;
echo "received_packets: ${Total_number_of_Erlang_Nodes}:${sum_received_packets}">>$Output_file_name;
echo "retransmission_packets: ${Total_number_of_Erlang_Nodes}:${sum_retransmission_packets}">>$Output_file_name;


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
echo "Finish benchmark at time">>$Output_file_name;
date +'%T'>>$Output_file_name;


