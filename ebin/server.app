{application, server,
    [{vsn, "1.0.0"},
     {modules, [server, server_sup, rclient]},
     {registered, []},
     {env, [   
            {listen_port, 8989},
            {acceptors, 20},
            {server, {{515,6384,50107,52498,26322,47880,52877,40926}}},
            {client1, 'abc@kushal'},
            {client2, 'def@kushal'},
            {cookie, gsoc},
            {ebin_dir, "/home/kushal/yggdrasil/ebin"}  % path to th ebin directory 
           ]
     },
     {mod, {server, []}}
    ]}.