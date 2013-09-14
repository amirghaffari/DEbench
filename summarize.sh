#!/bin/bash

# author: Amir Ghaffari
# @RELEASE project (http://www.release-project.eu

result_of="heriot_watt/SDErlang";
#result_of="uppsala";
Base_dir=`pwd`
Result_location="$Base_dir/results/${result_of}/"
summarize_location="$Base_dir/summarize_results/$result_of"

if [ ! -d "$summarize_location" ]; then
	mkdir -p $summarize_location;
else
	rm -rf $summarize_location/*
fi

for Dir in $(find ${Result_location}* -maxdepth 0 -type d ); 
do
	cd $Dir
	ParentDir=`basename $Dir`
	temp_summarize_location=$summarize_location/$ParentDir;
	mkdir -p $temp_summarize_location;
	for Dir in $(find ${Result_location}${ParentDir}/* -maxdepth 0 -type d ); 
	do
		cd $Dir
		for FILE in $(find . -maxdepth 1 -print | grep 'shell_output_nodes' );
		do
			cat $FILE | grep 'average_cpu_usage_percentage:' | awk  '{print $2}' >>$temp_summarize_location/cpu_basho_bench.txt
			cat $FILE | grep 'average_memory_usage_percentage:' | awk  '{print $2}' >>$temp_summarize_location/memory_basho_bench.txt
			cat $FILE | grep 'sent_packets:' | awk  '{print $2}' >>$temp_summarize_location/network_sent.txt
			cat $FILE | grep 'received_packets:' | awk  '{print $2}' >>$temp_summarize_location/network_received.txt
			cat $FILE | grep 'retransmission_packets:' | awk  '{print $2}' >>$temp_summarize_location/network_retransmission.txt
		done
	done

done
