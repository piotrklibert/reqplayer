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
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% External exports
-export([start_link/0]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([register/0]).
-export([register/1]).
-export([unregister/1]).

-record(state, {
    redis,
    redis_sub,
    listeners=[],
    filters=[]
}).

-define(CHANNEL, <<"newreq">>).

%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link/1
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


register() ->
    gen_server:call(?MODULE, {register, self()}).

register(Pid) ->
    gen_server:call(?MODULE, {register, Pid}).

unregister(Pid) ->
    gen_server:call(?MODULE, {unregister, Pid}).

% filter(Pid, SessionId) ->
%     ok.

%%====================================================================
%% Server functions
%%====================================================================

init([]) ->
    {ok, Client, Sub} = reqviewer:start_redis(),
    eredis_sub:controlling_process(Sub),
    eredis_sub:subscribe(Sub, [?CHANNEL]),
    {ok, #state{redis=Client, redis_sub=Sub}}.




handle_call({register, Pid}, _, State) ->
    {reply, ok, add_listener(Pid, State)};

handle_call({unregister, Pid}, _, State) ->
    {reply, ok, drop_listener(Pid, State)};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.



handle_cast(_Msg, State) ->
    {noreply, State}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Handling Redis PUB/SUB messages
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_info({subscribed, ?CHANNEL, Redis}, #state{redis_sub=Redis} = State) ->
    eredis_sub:ack_message(Redis),
    {noreply, State};

handle_info({message, Chan, Val, RedisSub},
            #state{redis_sub=RedisSub, redis=Redis} = State) ->
    lager:info("Got ~p from redis, state: ~s", [{Chan, Val}, t:fmt(State)]),

    % {Decoded} = jiffy:decode(Val),
    % {_, SessID} = lists:keyfind(<<"sess">>, 1, Decoded),
    % GetLast = ["lrange", SessID, "-1", "-1"],
    % {ok, Res} = eredis:q(Redis, GetLast),

    [Pid ! {text, Val} || Pid <- State#state.listeners],
    eredis_sub:ack_message(RedisSub),

    {noreply, State};

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
