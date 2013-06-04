* DEbench (Distributed Erlang Benchmark)

** Introduction

DEbench is a benchmark suite for distributed Erlang. It measures the throughput and latency of distributed Erlang commands and saves them in appropriate =CSV= files. DEbench's implementation is based on Basho Bensh (an open source benchmarking tool for Riak database).

You can find more details about Basho Bench here: (https://github.com/basho/basho_bench).

** How to build and run the benchmark suite 

1) Clean up after any previous builds (if necessary).

#+BEGIN_SRC shell
$ make clean
#+END_SRC

2) Build DEbench

#+BEGIN_SRC shell
$ git clone git://github.com/amirghaffari/DEbench
$ cd DEbench
$ make
#+END_SRC

3) Run DEbench: There are two ways to run the benchmark. Easier one is locally. To run it on a cluster, the cluster information is needed.

	3.1) To run localy:
		#+BEGIN_SRC shell
		$ ./de_bench bench.config
		#+END_SRC
	3.2) To run the benchmark on a cluster, you need to specify the cluster information (like number of nodes and nodes name) in files: run.sh and experiment.sh
		#+BEGIN_SRC shell
		$ ./run.sh
		#+END_SRC

Note) Benchmark's duration, commands and number of worker processes are defined in the config file.

Note) The config file for local runs is ="bench.config"=. After finishing the benchmark, the results are saved in ="test/current"= directory.

Note) The config file for clusters is ="template_bench.config"=. After finishing the benchmark, the results are saved in "results" directory.

4) Create graphs

	4.1) For local runs
		#+BEGIN_SRC shell
		$ make results
		#+END_SRC
		If everything goes well, you should have a graph (a png file) in ="test/current" directory=. 
	4.2) For clusters, after aggregating the CSV files for all nodes, the graph is created in "all/aggregated" directory.

Note) to create graphs, the R statistics language is needed. Find more about R at:  http://www.r-project.org/
