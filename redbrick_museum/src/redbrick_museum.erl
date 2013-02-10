-module(redbrick_museum).
-export([start/0]).

start() ->
	ok = application:start(ranch),
	ok = application:start(redbrick_museum).
