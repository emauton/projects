-module(user_protocol).
-export([start_link/4, init/4]).

start_link(ListenerPid, Socket, Transport, Opts) ->
	Pid = spawn_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]),
	{ok, Pid}.

init(ListenerPid, Socket, Transport, _Opts = []) ->
	ok = ranch:accept_ack(ListenerPid),
	loop(Socket, Transport).

loop(Socket, Transport) ->
	case Transport:recv(Socket, 0, infinity) of
		{ok, Data} ->
			Transport:send(Socket, colour:interpolate(binary_to_list(Data))),
			loop(Socket, Transport);
		_ ->
			ok = Transport:close(Socket)
	end.
