DEbench (Distributed Erlang Benchmark)
======================================

Introduction
------------

DEbench is a benchmark suite for distributed Erlang. It measures the throughput and latency of distributed Erlang commands and saves them in appropriate `CSV` files. 
DEbench's implementation is based on [Basho Bensh](https://github.com/basho/basho_bench) (an open source benchmarking tool for Riak database).

How to build and run the benchmark suite 
----------------------------------------

### Clean up after any previous builds (if necessary).

	$ make clean


### Build DEbench

	$ git clone git://github.com/amirghaffari/DEbench
	$ cd DEbench
	$ make


### Run DEbench

There are two ways to run the benchmark. Easier one is locally. To run it on a cluster, the cluster information is needed.

* To run localy:

		$ ./de_bench bench.config

* To run the benchmark on a cluster, you need to specify the cluster information (like number of nodes and nodes name) in files: `run.sh` and `experiment.sh`

		$ ./run.sh


Note) Benchmark's duration, commands and number of worker processes are defined in the config file.

Note) The config file for local runs is `bench.config`. After finishing the benchmark, the results are saved in `test/current` directory.

Note) The config file for clusters is `template_bench.config`. After finishing the benchmark, the results are saved in `results` directory.

### Create graphs

* For local runs

		$ make results

If everything goes well, you should have a graph (a png file) in `test/current` directory=. 

* For clusters, after aggregating the CSV files for all nodes, the graph is created in `all/aggregated` directory.

Note) to create graphs, the R statistics language is needed. Find more about [`R`](http://www.r-project.org/)
