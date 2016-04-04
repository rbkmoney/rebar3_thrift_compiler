-module(rebar3_thrift_compiler).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.

init(State0) ->
    {ok, State1} = rebar3_thrift_compiler_prv_compile:init(State0),
    {ok, State2} = rebar3_thrift_compiler_prv_clean:init(State1),
    {ok, State2}.
