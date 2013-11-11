-module(bench_put_log).
-export([run_bench/1]).

%% Time N messages through riak_kv_put_log, at full speed.
%% ~170k messages / second on my low-powered VM.
run_bench(N) ->
  riak_kv_put_log:start_link(),
  T0 = erlang:now(),
  bench_loop(N),
  T1 = erlang:now(),
  io:format("Time for ~p messages: ~pus~n", [N, timer:now_diff(T1, T0)]).

bench_loop(0) ->
  ok;
bench_loop(N) ->
  riak_kv_put_log:add({<<"Test bucket">>, <<"Test key">>}),
  bench_loop(N-1).
