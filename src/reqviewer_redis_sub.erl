%%%-------------------------------------------------------------------
%%% File    : reqviewer_redis_sub.erl
%%% Author  : Piotr Klibert <cji@fedorcia2>
%%% Description :
%%%
%%% Created : 16 Nov 2014 by Piotr Klibert <cji@fedorcia2>
%%%-------------------------------------------------------------------
-module(reqviewer_redis_sub).
-compile([{parse_transform, lager_transform}]).

-behaviour(gen_server).

%%--------------------------------------------------------------------
%% External exports
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).
-export([register/0, register/1, unregister/1, history/0, drop/0, purge/0]).


-record(state, {
    redis,
    redis_sub,
    queue,
    q_len = 0,
    listeners=[],
    filters=[]
}).


-define(CHANNEL, <<"newreq">>).

-define(DROP_STEP,     40).
-define(MAX_QUEUE_LEN, 45).
-define(MAX_MSG_SIZE,  10 * 1024).
-define(MAX_BODY_SIZE, 10 * 1024).


%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/1
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


register()      -> gen_server:call(?MODULE, {register, self()}).
register(Pid)   -> gen_server:call(?MODULE, {register, Pid}).
unregister(Pid) -> gen_server:call(?MODULE, {unregister, Pid}).
history()       -> gen_server:call(?MODULE, {history}).
drop()          -> gen_server:call(?MODULE, {drop, 1}).
purge()         -> gen_server:call(?MODULE, {drop, all}).


%%====================================================================
%% Server functions
%%====================================================================

init([]) ->
    {ok, Client, Sub} = reqviewer:start_redis(),
    eredis_sub:controlling_process(Sub),
    eredis_sub:subscribe(Sub, [?CHANNEL]),
    {ok, #state{redis=Client, redis_sub=Sub, queue=queue:new()}}.



handle_call({drop, all}, _, State) ->
    NewState = State#state{queue=queue:new(), q_len=1},
    {reply, ok, NewState};
handle_call({drop, N}, _, State) when is_number(N) ->
    {reply, ok, State#state{
        queue = drop_many(N, State#state.queue),
        q_len = State#state.q_len - N
    }};

handle_call({register, Pid}, _, State) -> {reply, ok, add_listener(Pid, State)};
handle_call({unregister, Pid}, _, State) -> {reply, ok, drop_listener(Pid, State)};
handle_call({history}, _, State) -> {reply, State#state.queue, State};

handle_call(_Request, _From, State) -> {reply, State, State}.



handle_cast(_Msg, State) ->
    {noreply, State}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Handling Redis PUB/SUB messages
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_info({subscribed, ?CHANNEL, Redis},
            #state{redis_sub=Redis} = State) ->
    eredis_sub:ack_message(Redis),
    {noreply, State};

handle_info({message, _Chan, Val, RedisSub},
            #state{redis_sub=RedisSub, redis=_Redis} = State) ->
    eredis_sub:ack_message(RedisSub),
    State2 = cache_message(State, Val),
    %% send received message to all registered listeners
    [Pid ! {text, Val} || Pid <- State2#state.listeners],
    {noreply, State2};

handle_info(_Info, State) ->
    {noreply, State}.



terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.



%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

add_listener(Pid, State = #state{listeners = Listeners}) ->
    lager:info("registering: ~p", [Pid]),
    State#state{listeners = [Pid | Listeners]}.

drop_listener(Pid, State = #state{listeners = Listeners}) ->
    lager:info("unregistering: ~p", [Pid]),
    State#state{listeners = Listeners -- [Pid]}.



%% Functions for maintaining cache/history of requests of a given length

%% Before caching we need to check if we have enough space in the queue; let's
%% drop some items before proceeding if not
cache_message(#state{queue=Q, q_len=L} = State, Val)
  when L >= ?MAX_QUEUE_LEN ->
    lager:info("Trimming history", []),
    cache_message(trim_queue(State), Val);

cache_message(#state{queue=Q, q_len=L} = State, Val) ->
    Val2 = if
        byte_size(Val) >= ?MAX_MSG_SIZE -> trim_message(Val);
        true                            -> Val
    end,
    State#state{
      queue = queue:in(Val2, Q),
      q_len = L+1
    }.



trim_queue(#state{queue=Q, q_len=L} = State) ->
    NewLength = if
        L - ?DROP_STEP < 0 -> 0;
        true -> L - ?DROP_STEP
    end,
    State#state{
        queue = drop_many(?DROP_STEP, Q),
        q_len = NewLength
    }.



trim_message(Val) ->
    JSON_Data = jiffy:decode(Val),
    JSON_Data2 = replace_too_long_values(
      [{{"req", "body"}, <<"Request body too long">>},
       {{"resp", "body"}, <<"Response body too long">>}],
      JSON_Data
    ),
    jiffy:encode(JSON_Data2).


replace_too_long_values([], JSON) -> JSON;
replace_too_long_values([ {Sel, Repl} | T ], JSON) ->
    KeyLength = get_js_val_size(ej:get(Sel, JSON)),
    if
        KeyLength > ?MAX_BODY_SIZE ->
            replace_too_long_values(T, ej:set(Sel, JSON, Repl));
        true ->
            replace_too_long_values(T, JSON)
    end.


get_js_val_size(undefined) -> 0;
get_js_val_size(V) when is_binary(V) -> byte_size(V);
get_js_val_size(V) when is_list(V) -> length(V).


drop_many(0, Queue) -> Queue;
drop_many(N, Queue) ->
    try
        Q2 = queue:drop(Queue),
        drop_many(N-1, Q2)
    catch
        _:empty -> Queue
    end.
