-module(eredis_mock_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).

-export([mock_test/1 ,all/0]).

all() -> [mock_test].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) ->
    {ok, Pid} = eredis_mock:start_link(),
    [{mock_pid, Pid} | Config].

end_per_testcase(_, Config) ->
    Pid = ?config(mock_pid, Config),
    eredis_mock:stop(Pid),
    Config.

mock_test(Config) ->
    Redis = ?config(mock_pid, Config),
    Request = ["SET", "key", "value"],
    Response = {error, error},
    ?assertEqual({ok, <<"OK">>}, eredis:q(Redis, Request)),
    eredis_mock:set_mock(Redis, Request, Response),
    ?assertEqual(Response, eredis:q(Redis, Request)),
    ?assert(true).
