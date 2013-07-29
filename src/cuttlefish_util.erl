-module(cuttlefish_util).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

-export([
	replace_proplist_value/3,
	replace_tuple_element/3,
	key_starts_with/2,
	variable_key_replace/2]).

replace_proplist_value(Key, Value, Proplist) ->
	proplists:delete(Key, Proplist) ++ [{Key, Value}].

replace_tuple_element(Index, Value, Tuple) ->
	list_to_tuple([
		case N =:= Index of
			true -> Value;
			_ -> element(N, Tuple)
		end
		|| N <- lists:seq(1, length(tuple_to_list(Tuple))) ]).

key_starts_with(Prefix, Proplist) ->
	lists:filter(
		fun({Key, _Value}) -> 
			string:str(Key, Prefix) =:= 1
		end, 
		Proplist).

variable_key_replace(Key, Sub) ->
    KeyTokens = string:tokens(Key, "."), 
    string:join([ begin 
        case hd(Tok) of
            $$ -> Sub;
            _ -> Tok
        end
    end || Tok <- KeyTokens], "."). 

%% TODO: keeping around for possible use in the advanced.config usecase. 
%% Priority is a nested set of proplists, but each list has only one item
%% for easy merge
%% merge([{K,V}]=Priority, Proplist) ->
%%     case proplists:get_value(K, Proplist) of
%%         undefined -> Proplist ++ Priority;
%%         Existing ->
%%             proplists:delete(K, Proplist) ++ merge(V, Existing) 
%%     end; 
%% merge([], Proplist) -> Proplist;
%% merge(Priority, []) -> Priority.

-ifdef(TEST).

replace_tuple_element_test() ->
	NewTuple = replace_tuple_element(3, "test", {1,2,3,4}),
	?assertEqual({1,2,"test",4}, NewTuple),
	ok.

replace_proplist_value_test() ->
	Proplist = [
		{"test1", 1},
		{"test2", 2},
		{"test3", 3}
	],

	NewProplist = replace_proplist_value("test2", 8, Proplist),
	?assertEqual(
		8,
		proplists:get_value("test2", NewProplist) 
		),
	ok.

key_starts_with_test() ->
	Proplist = [
		{"regular.key", 1},
		{"other.normal.key", 2},
		{"prefixed.key1", 3},
		{"prefixed.key2", 4},
		{"interleaved.key", 5},
		{"prefixed.key3", 6}
	],

	Filtered = key_starts_with("prefixed", Proplist),
	?assertEqual([
			{"prefixed.key1", 3},
			{"prefixed.key2", 4},
			{"prefixed.key3", 6}
		],
		Filtered),
	ok.
-endif.
