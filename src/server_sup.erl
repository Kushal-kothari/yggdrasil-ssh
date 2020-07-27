-module(server_sup).
-behaviour(supervisor).

-export([start_link/1, start_acceptor/0]).
-export([init/1]).

start_link(ListenSocket) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ListenSocket]).

init([ListenSocket]) ->
    {ok, {{simple_one_for_one, 60, 3600},
          [{socket,         % ChildId
            {server, start_link, [ListenSocket]}, 
            temporary,      % Restart strategy
            1000,           % Shutdown timeout
            worker,         % Type
            [server]}       % Module
          ]}}.

start_acceptor() ->
    supervisor:start_child(?MODULE, []).
