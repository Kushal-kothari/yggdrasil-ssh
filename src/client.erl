-module(client).
-behaviour(gen_server).

%% Client Application
-export([start/0, start/2, stop/1]).

%% Client API
-export([connections_start/1, connections_stop/1, count_connections/0]).

%% Client gen_server
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
         terminate/1]).


%%% Client application

start() ->
    io:format("starting ~w application~n", [?MODULE]),
    application:start(?MODULE).

start(normal, _Args) ->
    {ok, Sup} = client_sup:start_link(),
    {ok, Sup, []}.

stop(_State) ->
    ok.

%%% Client API %%%

connections_start(NoOfConnections) ->
    do_start_connections(NoOfConnections, []).

do_start_connections(0, Acc) ->
    Acc;
do_start_connections(NoOfConnections, Acc) ->
    {ok, ClientPid} = supervisor:start_child(client_sup, [NoOfConnections]),
    do_start_connections(NoOfConnections-1, [ClientPid|Acc]).

%% Stop the given number of connections
connections_stop(NoOfConnections) ->
    ChildDetails = supervisor:which_children(client_sup),
    Children = lists:map(fun({_Id, Pid, _Type, _Module}) -> Pid end, ChildDetails),
    do_stop_connections(NoOfConnections, Children).

do_stop_connections(0, _) ->
    ok;
do_stop_connections(_, []) ->
    io:format("less children than requested to stop");
do_stop_connections(NoOfConnections, [C|Children]) ->
    ok = supervisor:terminate_child(client_sup, C),
    do_stop_connections(NoOfConnections-1, Children).

count_connections() ->
    [_, _, _, {workers, Count}] = supervisor:count_children(client_sup),
    Count.


start_link(DelayFanout) ->
    gen_server:start_link(?MODULE, [DelayFanout], []).

init([DelayFanout]) ->
    process_flag(trap_exit, true),
    gen_server:cast(self(), connect),
    {ok, DelayFanout}.

handle_call(get_socket, _From, Socket) ->
    {reply, Socket, Socket}.

handle_cast(connect, DelayFanout) ->
    <<A:32, B:32, C:32>> = crypto:strong_rand_bytes(12),
    rand:seed({A,B,C}),

    {ok, Server} = application:get_env(client, server),
    {ok, Port} = application:get_env(client, listen_port),
    {ok, Interval} = application:get_env(client, interval),
    timer:sleep(rand:uniform(DelayFanout)),

    case gen_tcp:connect(Server, Port, [{active, false}]) of
        {ok, Socket} ->
            FirstInterval = rand:uniform(Interval),
            erlang:send_after(FirstInterval, self(), trigger),

            {noreply, Socket};
        {error, Reason} ->
            Reason1 = {shutdown, {Reason, 'gen_tcp:connect/3'}},
            {stop, Reason1, undefined}
    end;

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({tcp, _OtherSocket, _Msg}, Socket) ->
    io:format("client ~w received msg: ~s~n", [self(), _Msg]),
    ok = inet:setopts(Socket, [{active, once}]),
    {noreply, Socket};

handle_info(trigger, Socket) ->
    {ok, Timeout} = application:get_env(client, timeout),
    case gen_tcp:send(Socket, "Ping") of
        ok ->
            case gen_tcp:recv(Socket, 0, Timeout) of
                {ok, "Pong"} ->
                    {ok, Interval} = application:get_env(client, interval),
                    erlang:send_after(Interval, self(), trigger),
                    {noreply, Socket};
                {error, timeout} ->
                    Reason = {shutdown, {timeout, 'gen_tcp:recv/3'}},
                    {stop, Reason, Socket};
                {error, Reason} ->
                    Reason1 = {shutdown, {Reason, 'gen_tcp:recv/3'}},
                    {stop, Reason1, Socket}
            end;
        {error, Reason} ->
            Reason1 = {shutdown, {Reason, 'gen_tcp:send/3'}},
            {stop, Reason1, Socket}
    end;

handle_info(Info, State) ->
    io:format("unexpected: ~p~n", [Info]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Socket) ->
    io:format("client  terminated with:"),
    ok = gen_tcp:close(Socket).