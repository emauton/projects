% Roll up a set of https://pinboard.in/ pins for a blog post.
% Pulls down pins tagged "pinroll", picks the last N, makes a
% http://www.blogofile.com/ post with them.
-module(pinroll).
-export([main/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

% Exported functions.

% escript entry point.
main(Args) ->
  {ApiInfo, NumPins} = resolve_args_or_halt(Args),
  Pins = get_pins(ApiInfo, NumPins),
  {ok, Text} = post_dtl:render([{pins, Pins}]),
  io:format("~s", [Text]).

% Support functions.

% Use the pinboard API to pull down a list of N pins (as proplists).
-spec get_pins(pinboard:api_info(), non_neg_integer()) -> [pinboard:pin()].
get_pins(ApiInfo, N) ->
  pinboard:init(),
  All = pinboard:get(ApiInfo, "pinroll"),
  Pins = lists:sublist(lists:reverse(All), N),  % i.e. the last N pins.
  Untagged = [strip_pinroll_tag(P) || P <- Pins],
  ok = pinboard:update(ApiInfo, Untagged),
  Untagged.

% Strip the "pinroll" tag from a pin.
-spec strip_pinroll_tag(pinboard:pin()) -> pinboard:pin().
strip_pinroll_tag(Pin) ->
  Bin = proplists:get_value(<<"tags">>, Pin),
  Tags = string:tokens(binary_to_list(Bin), " "),
  NewTags = string:join([T || T <- Tags, T =/= "pinroll"], " "),
  [{<<"tags">>, list_to_binary(NewTags)} | proplists:delete(<<"tags">>, Pin)].

options() ->
  [
    {api_end_point, $e, "api-end-point", string,
      "URL of base pinboard API; default https://api.pinboard.in/v1"},
    {auth_token, $t, "auth-token", string,
      "File to load pinboard auth token from; default ~/.pintoken"},
    {help, $h, "help", undefined, "Print usage"},
    {num_pins, $n, "num-pins", integer,
      "Number of pinboard pins to fetch; default 3"}
  ].

% The following functions support main() and may halt.
% This is straightforward commandline arg checking, tested by hand.
print_usage_and_halt(Status) ->
  getopt:usage(options(), escript:script_name()),
  halt(Status).

resolve_args_or_halt(Args) ->
  Opts = case getopt:parse(options(), Args) of
    {ok, {Options, _}} ->
      Options;
    {error, {Reason, Data}} ->
      io:format(standard_error, "Failed to parse arguments: ~p: ~p~n",
                [Reason, Data]),
      print_usage_and_halt(1)
  end,
  case lists:member(help, Opts) of
    true ->
      print_usage_and_halt(0);
    false ->
      ok
  end,
  ApiEndPoint = proplists:get_value(api_end_point, Opts,
                                    "https://api.pinboard.in/v1"),
  TokenFile = proplists:get_value(auth_token, Opts,
                                  filename:join([os:getenv("HOME"),
                                                 ".pintoken"])),
  NumPins = proplists:get_value(num_pins, Opts, 3),
  AuthToken = read_token(TokenFile),
  {{ApiEndPoint, AuthToken}, NumPins}.

read_token(TokenFile) ->
  case file:read_file(TokenFile) of
    {error, Reason} ->
      io:format(standard_error, "Failed to load auth_token file: ~p: ~p~n",
            [TokenFile, Reason]),
      print_usage_and_halt(1);
    {ok, Data} -> string:strip(binary_to_list(Data), right, $\n)
  end.

-ifdef(TEST).

% Get N fixtures - with and without the "pinroll" tag.
get_pins_fixtures(N) -> 
  % get_pins manipulates only the "tags" value of the proplist (via
  % strip_pinroll_tags), so for fixtures we can get by with these minimal
  % proplists.
  Pins     = [
               [{<<"tags">>, <<"tag1 tag2 pinroll">>}],
               [{<<"tags">>, <<"tag3 tag4 pinroll">>}]
             ],
  Untagged = [
               [{<<"tags">>, <<"tag1 tag2">>}],
               [{<<"tags">>, <<"tag3 tag4">>}]
             ],
  {lists:sublist(Pins, N), lists:sublist(Untagged, N)}.

% Test get_pins() with parameterized fixtures:
%   Pins - returned by pinboard;
%   Untagged - pins after filtering for the last N;
%   N - number of pins requested.
get_pins_with(Pins, Untagged, N) ->
  meck:new(pinboard),
  meck:expect(pinboard, init, fun() -> ok end),
  meck:expect(pinboard, get,
              fun(_ApiInfo, "pinroll") ->
                Pins
              end),
  meck:expect(pinboard, update,
              fun(_ApiInfo, P) ->
                ?assertEqual(Untagged, P),
                ok 
              end),
  ?assertEqual(Untagged, get_pins({endpoint, token}, N)),
  ?assert(meck:validate(pinboard)),
  meck:unload(pinboard).

get_pins_test() ->
  % Test: 0 pins returned by pinboard.
  get_pins_with([], [], 3),

  % Test: 1 < N pins returned by pinboard.
  {Pins1, Untagged1} = get_pins_fixtures(1),
  get_pins_with(Pins1, Untagged1, 3),

  % Test: M > N pins returned by pinboard.
  % Pull 2 pins; only the latter (i.e. the "last 1" should be
  % used and returned by get_pins().
  {PinsM, [_, Last1] } = get_pins_fixtures(2),
  UntaggedM = [Last1],
  get_pins_with(PinsM, UntaggedM, 1).

-endif.
