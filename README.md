# Yggdrasil

A little Erlang application.


## Configuration

Configuration parameters like the server IP address, the listening port, the client hostnames etc need to be added  through a configuration file, see the `yggdrasil.config`.


# Remote Client

An  remote client (`rclient`) is available, which uses Erlang's `slave` module and to start a remote node and the client application on that node. This way it is possible to control a server and multiple clients from one Erlang console.

This setup assumes
* that the whole application (source code, binaries, app files etc) is available at all nodes
* passwordless ssh access to remote nodes
* hostnames are configured correctly


## Usage

Compile the sources with `erl -make`. Start the server application with
```
erl -rsh ssh -name master -pa ebin -s server -config yggdrasil.config
```
(assuming a configuration file `yggdrasil.config`). This starts the server application, which starts the supervisor and the configured number of acceptors.
From the console, start a client application on a remote node with
```
Client1 = rclient:start(client1).
```
`client1` needs to a key in the configuration of the `yggdrasil` application (the `env` part in the `.app` file), indicating the hostname of the remote node designated to run the client. This starts a remote node using the Erlang's `slave` module and starts the client application on that node. Multiple client applications can be started if more hosts are available.

To start for example 10 connections from `Client1` use:
```
rclient:connections_start(10, Client1).
```

To stop connections, use `yggdrasil:connections_stop/2`, e.g.
```
rclient:connections_stop(5, Client1).
```



