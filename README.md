DE-Bench (Distributed Erlang Benchmark)
-----------

Introduction
------------

DE-Bench is a benchmark tool for distributed Erlang. It measures the throughput and latency of distributed Erlang commands and saves them in appropriate `CSV` files. In DE-Bench the latency and throughput of the following commands can be measured:
* Spawn: spawns a function on a local/remote node.
* RPC: calls a function on a local/remote node.
* Globally or locally name registration: A PID (process identifier) is registered, looked up, and unregistered globally or locally. Functions `register`, `whereis`, `unregister` are called respectively.
* Synchronous call to a server process: A function from an OTP `gen_server` or `gen_fsm` is called. Functions `gen_server:call` and `gen_fsm:sync_send_event` are used.

**Note**) in cases that a function is called, i.e. `spawn`, `RPC`, `gen_server:call` and `gen_fsm:sync_send_event`, the argument’s size of the function and the time that function takes to complete is configurable. For example, we can specify in the configuration file that function’s argument size is 200 bytes and function takes 10 microseconds to complete on target node.

**More**) We have used DE-Bench in the [RELEASE project] (http://www.release-project.eu/) to measure the scalability of distributed Erlang. You can find more in the report (`report.pdf`).

DE-Bench's implementation is based on [Basho Bensh](https://github.com/basho/basho_bench) (an open source benchmarking tool for Riak database).

How to build and run the benchmark suite 
----------------------------------------


### Build DE-Bench

	$ git clone git://github.com/amirghaffari/DEbench
	$ cd DEbench
	$ make

### Clean up after any previous builds (if necessary).

	$ make clean

### Run DE-Bench

There are two ways to run the benchmark. Easier one is local run in which all commands are run on the local node (`nonode@nohost`). To run the benchmark on a cluster, the cluster information is needed.

* To run localy:

		$ ./de_bench bench.config

The config file for local runs is `bench.config`. After finishing the benchmark, the results are saved in `test/current` directory.

* To run the benchmark on a cluster, you need to specify the cluster information (like number of nodes and nodes name) in files: `run.sh` and `experiment.sh`

		$ ./run.sh

The config file for clusters is `template_bench.config`. After finishing the benchmark, the results are saved in `results` directory.

Note) Benchmark's duration, commands and number of worker processes are defined in the config file.


### Create graphs

* To create graph for local runs

		$ make results

If everything goes well, you should have a graph (a png file) in `test/current` directory.

* For clusters, after aggregating the CSV files for all nodes, the graph is created in `all/aggregated` directory.

**Note**) to create graphs, the R statistics language is needed. Find more about [`R`](http://www.r-project.org/)
