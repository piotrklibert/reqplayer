-module(reqviewer_handler).

-compile([{parse_transform, lager_transform}]).
-export([init/2]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([terminate/3]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Regular HTTP request handler
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Prepare WebSocket connection
init(Req, Opts) ->
    lager:info("New WebSocket connection with: ~s", [t:fmt(Req)]),
    reqviewer_redis_sub:register(self()),
    {cowboy_websocket, Req, Opts}.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% WebSocket connection handler
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% Handler for events coming via WebSocket
%%--------------------------------------------------------------------

websocket_handle({text, <<"init">>}, Req, State) ->
    {reply, {text, <<"ok">>}, Req, State};

websocket_handle({text, Msg}, Req, State) ->
    lager:info("websocket_handle message arrived: ~p", [Msg]),
    lager:info("~s ~p", [t:fmt(Req), State]),
    Resp = {[
        {success, true},
        {errors, []}
    ]},
	{reply, {text, jiffy:encode(Resp)}, Req, State};

websocket_handle(_Data, Req, State) ->
	{ok, Req, State}.


%%--------------------------------------------------------------------
%% Handler for events coming from Redis
%%--------------------------------------------------------------------

%% We got a notification from Redis, new Trace has arrived. We're just passing
%% it to the client as is, without any changes.
websocket_info({text, TraceData}, Req, State) ->
    % io:format("~p", [jiffy:decode(TraceData)]),
    {reply, {text, TraceData}, Req, State};

websocket_info(ping, Req, State) ->
    {reply, {text, "pong"}, Req, State};

websocket_info(Msg, Req, State) ->
    lager:warning("Unrecognized input for websocket_info: ~s", [t:fmt(Msg)]),
	{ok, Req, State}.


%%--------------------------------------------------------------------
%% Handler called when client disconnects
%%--------------------------------------------------------------------

terminate(_Reason, _Req, _State) ->
    reqviewer_redis_sub:unregister(self()),
    ok.
