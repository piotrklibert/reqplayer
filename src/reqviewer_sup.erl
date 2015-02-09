-module(reqviewer_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).



%% ===================================================================
%% API functions
%% ===================================================================

start_link(_Pid) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).



%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, {
       {one_for_one, 5, 10}, [
           {reqviewer_redis_sub,
            {reqviewer_redis_sub, start_link, []},
            permanent, 5000, worker, [reqviewer_redis_sub] },
            {data_store,
            {data_store, start_link, []},
            permanent, 5000, worker, [data_store] }]
      }}.
