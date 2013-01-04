% A simple RFC 1413 ident client.
-module(ident).
-export([start/0, request/1, request/3]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-define(IDENT_PORT, 113).

% Client API.
start() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% request(Sock) |
% request(RemoteAddr, RemotePort, LocalPort) ->
%   "username" | unknown | {error, Reason}
%  Makes a request of the RFC 1413 ident server (either on the connected
%  peer of Sock or on RemoteAddr) to lookup the user associated with the
%  connection.
request(Sock) ->
  gen_server:call(?MODULE, {request, Sock}).
request(RemoteAddr, RemotePort, LocalPort) ->
  gen_server:call(?MODULE, {request, RemoteAddr, RemotePort, LocalPort}).

% This gen_server's state is just the compiled matching regex.
% Matches e.g. "51131,22:USERID:UNIX:cian\r\n".
init(_Args) ->
  re:compile("^.*:USERID:UNIX:(.*)\r$").

% Implementation of request/1 & request/3.
handle_call({request, Sock}, _From, State) ->
  case inet:peername(Sock) of
    {ok, {RemoteAddr, RemotePort}} ->
      case inet:sockname(Sock) of
        {ok, {_, LocalPort}} ->  
          handle_call({request, RemoteAddr, RemotePort, LocalPort}, _From, State);
        {error, What} ->
          {reply, {error, {inet_sockname, What}}, State}
      end;
    {error, What} ->
      {reply, {error, {inet_peername, What}}, State}
  end;

handle_call({request, RemoteAddr, RemotePort, LocalPort}, _From, State) ->
  case sendrecv(RemoteAddr, ?IDENT_PORT,
                io_lib:format("~w , ~w\r\n", [RemotePort, LocalPort]))
  of
    {ok, Data} ->
      case re:run(Data, State, [{capture, [1], list}]) of
        {match, [Username]} ->
          {reply, Username, State};
        nomatch ->
          {reply, unknown, State}
      end;
    {error, What} ->
      {reply, {error, {sendrecv, What}}, State}
  end.

% sendrecv(Address, Port, Message) -> {ok, Data} | {error, Reason}
%  Connect to Address:Port, send a single message, and return the response.
%  Any errors are propagated directly from the gen_tcp functions.
sendrecv(Address, Port, Message) ->
  case gen_tcp:connect(Address, Port, [{active, false}]) of
    {ok, Sock} ->
      case gen_tcp:send(Sock, Message) of
        ok ->
          Response = gen_tcp:recv(Sock, 0),
          gen_tcp:close(Sock),
          Response;
        Error ->
          gen_tcp:close(Sock),
          Error
      end;
    Error ->
      Error 
  end.

% gen_server boilerplate, unused for now.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
