-module(eredis_mock).
-behavior(gen_server).

%% API exports
-export([start_link/0,stop/1,set_mock/3,clean/1]).

%% gen_server exports
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

-record(st, {mocks = #{}}).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> {ok,pid()} | ignore | {error,any()}.
start_link() ->
    gen_server:start_link(?MODULE, [], []).

-spec stop(Pid :: pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid, normal, 10).

-spec set_mock(Pid :: pid(), Request :: iolist(), Response :: {ok, binary()} | {error, any()}) -> ok.
set_mock(Pid, Request, Response) ->
    gen_server:cast(Pid, {set_mock, Request, Response}).

-spec clean(Pid :: pid()) -> ok.
clean(Pid) ->
    gen_server:cast(Pid, clean).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
    {ok, #st{}}.

handle_call({request, Request}, _From, #st{mocks = Mocks} = State) ->
    {reply, maps:get(Request, Mocks, {ok, <<"OK">>}), State};
handle_call(_Msg, _From, State) ->
    {reply, {ok, <<"OK">>}, State}.

handle_cast({set_mock, Request, Response}, #st{mocks = Mocks}) ->
    Bulk = eredis:create_multibulk(Request),
    {noreply, #st{mocks = maps:put(Bulk, Response, Mocks)}};
handle_cast(clean, _State) ->
    {noreply, #st{}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, _State) ->
    {stop, Reason}.

%%====================================================================
%% Internal functions
%%====================================================================
