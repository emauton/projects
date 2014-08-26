% pinboard: A minimal interface to https://pinboard.in/api/ for 'pinroll'.
-module(pinboard).
-export([init/0, get/2, update/2]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type api_info() :: {string(), string()}.
-type pin() :: [proplists:property(), ...].
-export_type([api_info/0, pin/0]).

init() ->
  ssl:start(),
  ibrowse:start().

% Get a list of all pins with the given tag.
-spec get(api_info(), string()) -> [pin()].
get({ApiEndPoint, AuthToken}, Tag) ->
  Query = [ApiEndPoint, "/posts/all?format=json&auth_token=",
           AuthToken, "&tag=", Tag],
  JSON = request(Query),
  jsx:decode(JSON).

% Add a list of pins, replacing any that exist already.
-spec update(api_info(), [pin()]) -> ok.
update(_, []) ->
  ok;
update({ApiEndPoint, AuthToken} = ApiInfo, [Pin | Pins]) ->
  % We want only a subset of the pin's k,v pairs, and further need to translate
  % into the pinboard 'add' API's terms. Kind of annoying.
  PinParams = [api_translate(Key, Pin) || Key <- [<<"href">>,
                                                  <<"description">>,
                                                  <<"extended">>,
                                                  <<"tags">>,
                                                  <<"time">>,
                                                  <<"shared">>,
                                                  <<"toread">>]],

  % Add a few final parameters before piping the IOList through io_lib.
  Query = [[ApiEndPoint, "/posts/add?replace=yes&auth_token=", AuthToken, $&]
           | PinParams],
  request(Query),
  update(ApiInfo, Pins).

% Translate pin keys to their delicio.us API counterparts,
% urlencode pin values, and return both as a URL parameter.
-spec api_translate(binary(), pin()) -> iolist().
api_translate(Key, Pin) ->
  Value = proplists:get_value(Key, Pin),
  Encoded = ibrowse_lib:url_encode(binary_to_list(Value)),
  ApiKey = case Key of
    <<"href">> -> <<"url">>;
    <<"time">> -> <<"dt">>;
    Other  -> Other
  end,
  [ApiKey, $=, Encoded, $&].

% Make a request to a URL (specified as an IOList); return the response body.
-spec request(iolist()) -> binary().
request(IOList) ->
  Url = lists:flatten(io_lib:format("~s", [IOList])),
  {ok, "200", _, Data} = ibrowse:send_req(Url, [], get, [],
                                          [{ssl_options, []}, {is_ssl, true},
                                           {response_format, binary}]),
  Data.

-ifdef(TEST).

get_test() ->
  ApiInfo = {"http://example.com", "TOKEN"},
  ExpectedUrl = "http://example.com/posts/all?format=json&auth_token=TOKEN&tag=pinroll",
  JSON = <<"[\"test data\"]">>,
  ExpectedData = [<<"test data">>],
  meck:new(ibrowse),
  meck:expect(ibrowse, send_req,
              fun(Url, _, get, _, _) ->
                ?assertEqual(ExpectedUrl, Url),
                {ok, "200", headers, JSON}
              end),
  ?assertEqual(ExpectedData, get(ApiInfo, "pinroll")),
  ?assert(meck:validate(ibrowse)),
  meck:unload(ibrowse).

update_test() ->
  % Adequately covered by api_translate_test() and request_test().
  ok.

api_translate_test() ->
  Pin = [{<<"href">>, <<"some url">>}, {<<"other">>, <<"some data">>}],
  HrefExpected  = [<<"url">>, $=, "some+url", $&],
  OtherExpected = [<<"other">>, $=, "some+data", $&],
  ?assertEqual(HrefExpected, api_translate(<<"href">>, Pin)),
  ?assertEqual(OtherExpected, api_translate(<<"other">>, Pin)).

request_test() ->
  Query = ["http://example.com", $/, "posts/all", $?, <<"param=">>, "some+data"],
  Expected = "http://example.com/posts/all?param=some+data",
  Data = <<"data">>,
  meck:new(ibrowse),
  meck:expect(ibrowse, send_req,
              fun(Url, _, get, _, _) ->
                ?assertEqual(Expected, Url),
                {ok, "200", headers, Data}
              end),
  ?assertEqual(Data, request(Query)),
  ?assert(meck:validate(ibrowse)),
  meck:unload(ibrowse).

-endif.
