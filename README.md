eredis_mock
=====

It is a simple library for mocking eredis queries in tests.
It really terrible to use real redis for tests, especially for unit-tests, so this library allows to mock particular queries for eredis.

Example
-----

```erlang
    {ok, Pid} = eredis_mock:start_link(),
    Request = ["SET", "key", "value"],
    Response = {ok, <<"MOCK">>},
    eredis_mock:set_mock(Redis, Request, Response),
    ?assertEqual(Response, eredis:q(Redis, Request)),
```

Build
-----

    $ rebar3 compile
