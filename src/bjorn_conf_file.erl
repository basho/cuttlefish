-module(bjorn_conf_file).

-export([file/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

-spec file(string()) -> [{string(), any(), list()}].
file(Filename) ->
    {ok, B} = file:read_file(Filename),
    %% TODO: Hardcoded utf8
    S = unicode:characters_to_list(B, utf8),
    string(S).

string(S) ->
	string_l(string:tokens(S, [$\n])).

string_l([[$#|_Line]|Lines]) ->
	string_l(Lines);
string_l([Line|Lines]) ->
	[Key, Value] = [string:strip(X) || X <- string:tokens(Line, "=")],
	[{Key, Value}| string_l(Lines)];
string_l([]) -> [].

-ifdef(TEST).
file_test() ->
    Conf = file("../test/riak.conf"),
    ?assertEqual(
    	[
    		{"anti_entropy", "on"},
			{"ring_size", "32"}
    	],
    	Conf),
    ok.
-endif.