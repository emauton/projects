#!/bin/sh
erl -pa ebin deps/*/ebin -s redbrick_museum -eval "io:format(\"Run: telnet localhost 5555~n\")."
