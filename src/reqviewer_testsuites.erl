-module(reqviewer_testsuites).

-export([init/2]).

init(Req, Opts) ->
    Action = erlang:binary_to_atom(proplists:get_value(<<"action">>, cowboy_req:parse_qs(Req), <<"index">>), utf8),
    [Data] = dispatch_action(Action, Req),
    Req2 = cowboy_req:reply(200, [], jiffy:encode({Data}), Req),
    {ok, Req2, Opts}.


dispatch_action(index, Req) ->
    data_store:index();

dispatch_action(addtest, Req) ->
    ReqQS = cowboy_req:parse_qs(Req),
    SuiteId = proplists:get_value(<<"suite_id">>, ReqQS),
    TestData = proplists:get_value(<<"test_data">>, ReqQS),
    io:format("~p~p", [SuiteId, TestData]),
    data_store:add_test(SuiteId, TestData).
