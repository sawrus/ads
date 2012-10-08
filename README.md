# ADS

ADS is an HTTP(S) server which calculate and stores statistics data. 
This server builded on misultin modules and eredis as NoSQL engine.

https://github.com/sawrus/ads

# Features

 * Flexible way for setting server variables
 * Apply Restful way for handling the input GET requests
 * OOB support to work in SSL mode
 * Ability to Extend and change statistics data via env variables (inflight changes)
 * Ability to upload files with any format (power on cache mode for html files)
 * If cached html file was changed ie size was changed then this file will be put in NoSQL cache server for calculate and store statistics data.

# Quick Start


 The typical 'Hello World" example code is:

```erlang

% application start callback
application:start(ads).

```

# Module Exports

The complete list of module exports can be found [here](https://github.com/ostinelli/misultin/tree/master/EXPORTS.md).

# Dependencies

You will need:

 * [Erlang](http://www.erlang.org/download.html) >= R14B01
 * [Rebar](https://github.com/basho/rebar) to compile
 * [Redis](http://redis.io/download) to compile
 * [Eredis](https://github.com/wooga/eredis.git)
 * [Misultin](http://github.com/ostinelli/misultin.git)

