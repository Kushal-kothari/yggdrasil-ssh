{application, client,
    [{vsn, "1.0.0"},
     {modules, [client, client_sup]},
     {registered, []},
     {env, [
            {listen_port, 8989},
            {server, {515,6384,50107,52498,26322,47880,52877,40926}},
            {interval, 5000},
            {timeout, 1000}
           ]
     },
     {mod, {client, []}}
    ]}.