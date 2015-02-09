%%% The core of the app: the server in charge of tracking processes.
-module(data_store).
-behaviour(gen_server).

-export([start_link/0, stop/0, index/0, add_test/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

%%%%%%%%%%%%%%%%%
%%% INTERFACE %%%
%%%%%%%%%%%%%%%%%
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

%% Give a name to a process
add_test(SuiteId, TestData) ->
    gen_server:call(?MODULE, {add_test, SuiteId, TestData}).

index() ->
    gen_server:call(?MODULE, index).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GEN_SERVER CALLBACKS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([]) ->
    TestSuites = dets:open_file(suites, []).

handle_call(index, _From, TestSuites) ->
    {reply, dets:match(TestSuites, '$1'), TestSuites};

handle_call({add_test, SuiteId, TestData}, _From, TestSuites) ->
    CurrentSuite = dets:lookup(TestSuites, SuiteId),
    dets:insert(TestSuites, {SuiteId, [TestData|CurrentSuite]}),
    io:format("kjskljds~n"),
    {reply, dets:match(TestSuites, '$1'), TestSuites};



handle_call(_Event, _From, State) ->
    {noreply, State}.

handle_cast(_Event, State) ->
    {noreply, State}.

handle_info(_Event, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.


