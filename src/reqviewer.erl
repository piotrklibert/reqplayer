-module(reqviewer).
-compile(export_all).


start_redis() ->
    {ok, Port}   = application:get_env(reqviewer, redis_port),
    {ok, Host}   = application:get_env(reqviewer, redis_host),
    {ok, Client} = eredis:start_link(Host, Port),
    {ok, Sub}    = eredis_sub:start_link(Host, Port, ""),
    {ok, Client, Sub}.
