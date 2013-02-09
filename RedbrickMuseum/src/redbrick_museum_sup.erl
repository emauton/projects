-module(redbrick_museum_sup).
-export([start_link/0]).

-behaviour(supervisor).
-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ok, {{one_for_one, 10, 10}, []}}.
