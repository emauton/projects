-module(redbrick_museum_app).

-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
	{ok, _} = ranch:start_listener(redbrick_museum, 1,
		ranch_tcp, [{port, 5555}], user_protocol, []),
	redbrick_museum_sup:start_link().

stop(_State) ->
	ok.
