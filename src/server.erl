-module(server).
-behaviour(gen_server).

-export([start/2, stop/1]).

-export([start/0]).

% gen_server api
-export([start_link/1, count_connections/0]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
         terminate/2]).


%%% Application Callbacks %%%%

%% Start the yggdrasil app. Opens a listen port at the port defined in the env
%% and opens Acceptors number of concurrent acceptors.
start(normal, _Args) ->
    {ok, Port} = application:get_env(listen_port),
    {ok, Acceptors} = application:get_env(acceptors),
    {ok, Backlog} = application:get_env(backlog),
    {ok, Yggdrasil} = application:get_env(yggdrasil),
    io:format("listening on port ~w with backlog of ~w~n", [Port, Backlog]),
    case re:run(Yggdrasil,"20") of
    {match,[{0,2}]} -> {ok,Parsed_add} = inet:parse_address(Yggdrasil),
                       {ok, ListenSocket} = gen_tcp:listen(Port, [{active,once}, {reuseaddr, true},{ip,Parsed_add}, {backlog, Backlog}]),
                       {ok, Sup} = server_sup:start_link(ListenSocket),
                        _ = [ supervisor:start_child(Sup, []) || _ <- lists:seq(1,Acceptors)],
                       {ok, Sup, ListenSocket};
    {match,_Rest} -> io:format("Not a Yggdrasil address");                     
    nomatch -> io:format("cannot establish connection : Check your Yggdrasil Address or if Port blocked by firewall")
     end.                

%% Stop the yggdrasil app (closes the listen socket)
stop(ListenSocket) ->
    ok = gen_tcp:close(ListenSocket),
    ok.

start() ->
    io:format("starting Yggdrasil ~w application~n", [?MODULE]),
    application:start(?MODULE).


%%% API %%%%%%

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

count_connections() ->
    [_, _, _, {workers, Count}] = supervisor:count_children(server_sup),
    {ok, Acceptors} = application:get_env(server, acceptors),
    Count - Acceptors.


%%% gen_server callbacks %%%%%%

init(Socket) ->
    process_flag(trap_exit, true),
    gen_server:cast(self(), accept),
    {ok, Socket}.

handle_call(get_socket, _From, Socket) ->
    {reply, Socket, Socket};

handle_call(_E, _From, State) ->
    {noreply, State}.

handle_cast(accept, ListenSocket) ->
    io:format("~w starting acceptor~n", [self()]),
    case gen_tcp:accept(ListenSocket) of
        {ok, AcceptSocket} ->
            {ok, {ServerIp, ServerPort}} = inet:sockname(AcceptSocket),
            {ok, {ClientIp, ClientPort}} = inet:peername(AcceptSocket),
            io:format("serv ~w accepted connection <Server ~w:~w> <Client ~w:~w>~n",
                      [self(), ServerIp, ServerPort, ClientIp, ClientPort]),
            server_sup:start_acceptor(), 
            {noreply, AcceptSocket};
        {error, Reason} ->
            Reason1 = {shutdown, {Reason, 'gen_tcp:accept/1'}},
            {stop, Reason1, ListenSocket}
    end.

handle_info({tcp, _Socket, "Ping"}, Socket) ->
    ok = gen_tcp:send(Socket, "Pong"),
    ok = inet:setopts(Socket, [{active, once}]),
    {noreply, Socket};

handle_info({tcp, _Socket, Str}, Socket) ->
    io:format("Received message: ~s~n", [Str]),
    {noreply, Socket};

handle_info({tcp_closed, _Socket}, S) ->
    {stop, {shutdown, tcp_closed}, S};
handle_info({tcp_error, _Socket, Error}, S) ->
    io:format("tcp_error: ~p~n", [Error]),
    {stop, {tcp_error, Error}, S};
handle_info(E, S) ->
    io:format("unexpected: ~p~n", [E]),
    {noreply, S}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, Socket) ->
    ok = gen_tcp:close(Socket),
    io:format("serv ~w terminated with reason: ~p~n", [self(), Reason]).