# ADS

ADS is an HTTP(S) server which calculate and stores statistics data. 

https://github.com/sawrus/ads

# Features

 * Flexible way for setting server variables
 * Apply Restful way for handling the input GET requests
 * OOB support to work in SSL mode
 * Ability to Extend and change statistics data via env variables (inflight changes)
 * Ability to upload files with any format (power on cache mode for html files)
 * If cached html file was changed ie size was changed then this file will be put in NoSQL cache# Quick Start


 The typical 'Hello World" example code is:

```erlang

% application start callback
application:start(ads).

```
 
# Installation notes

  You should make the next steps:

 * Need to install openssl (>= 1.0.1) [OpenSSL](http://www.openssl.org/source/openssl-1.0.1c.tar.gz)
 * Need to install erlang (>=R14B01)
 * Need to install redis 
 * Download the last stable ADS release [ADS](https://github.com/downloads/sawrus/ads/ads_v0.1.tar.gz)
 * Unzip archive: tar -xzf ads_vMj.Mi.tar.gz
 * Run it: As daemon via (./bin/ads start) or As console via (/bin/ads console)

# Module Exports

The complete list of module exports can be found [here](https://github.com/ostinelli/misultin/tree/master/EXPORTS.md).

# Dependencies

 You will need:

 * [Erlang](http://www.erlang.org/download.html) >= R14B01
 * [Rebar](https://github.com/basho/rebar) to compile
 * [Redis](http://redis.io/download) to compile
 * [Eredis](https://github.com/wooga/eredis.git)
 * [Misultin](http://github.com/ostinelli/misultin.git)

