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

%% @doc Log a {bucket, key} message.
add(BKey) ->
  gen_server:cast(?MODULE, {add, BKey}).

%% gen_server
%% State = {Fd, Count, Time} where
%%   Fd = log file descriptor,
%%   Count = number of messages logged,
%%   Time = time of log file creation.
%% We allow failed file operations to crash the server.
init([]) ->
  {ok, {create(), 0, seconds()}}.

handle_cast({add, {Bucket, Key}}, State) ->
  {Fd, Count, Time} = maybe_rotate(State),
  {Mega, Seconds, Micro} = os:timestamp(),
  file:write(Fd, [integer_to_list(Mega), $, , integer_to_list(Seconds), $, ,
                  integer_to_list(Micro), ":::", Bucket, $, , Key, $\n]),
  {noreply, {Fd, Count+1, Time}}.

%% Internal.
seconds() ->
  {_Megaseconds, Seconds, _Microseconds} = os:timestamp(),
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
  Filename = ["./log/put_log.", integer_to_list(seconds())],
  {ok, Fd} = file:open(Filename, [append, raw, delayed_write]),
  Fd.

%% Not implemented.
handle_call(_Args, _From, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
