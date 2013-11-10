%% @doc A simple rotating logger to write a log of updated keys on each vnode.
%%      Rotates either at 1 hour intervals or after 2**20 messages.
%%      Logfile cleanup is left to external automation.
%%      Quick and dirty PoC.
-module(riak_kv_put_log).
-behaviour(gen_server).

%% API
-export([start_link/0, add/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Log a message.
add(Message) ->
  gen_server:cast(?MODULE, {add, Message}).

%% gen_server
%% State = {Fd, Count, Time} where
%%   Fd = log file descriptor,
%%   Count = number of messages logged,
%%   Time = time of log file creation.
%% We allow failed file operations to crash the server.
init([]) ->
  {ok, {create(), 0, seconds()}}.

handle_cast({add, Message}, State) ->
  {Fd, Count, Time} = maybe_rotate(State),
  file:write(Fd, io_lib:format("~p:::~4096p~n", [erlang:now(), Message])),
  {noreply, {Fd, Count+1, Time}}.

%% Internal.
seconds() ->
  {_Megaseconds, Seconds, _Milliseconds} = erlang:now(),
  Seconds.

maybe_rotate({Fd, Count, Time}) ->
  Delta = seconds() - Time,
  if
    Delta > 3600 ; Count > 1024*1024 ->
      NewFd = rotate(Fd),
      {NewFd, 0, seconds()};
    true ->
      {Fd, Count, Time}
  end.

rotate(Fd) ->
  ok = file:close(Fd),
  create().

create() ->
  Filename = io_lib:format("./log/put_log.~b", [seconds()]),
  {ok, Fd} = file:open(Filename, [append, raw, delayed_write]),
  Fd.

%% Not implemented.
handle_call(_Args, _From, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
