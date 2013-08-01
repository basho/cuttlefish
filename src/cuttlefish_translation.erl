-module(cuttlefish_translation).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

-record(translation, {
	mapping::string(),
	func::fun()
	}).
-opaque translation() :: #translation{}.
-export_type([translation/0]).

-export([
	parse/1,
	is_translation/1,
	mapping/1, 
	func/1]).

-spec parse({translation, string(), fun()}) -> translation().
parse({translation, Mapping, Fun}) ->
	#translation{
		mapping = Mapping,
		func = Fun
	}.

-spec is_translation(any()) -> boolean().
is_translation(T) -> is_tuple(T) andalso element(1, T) =:= translation.

-spec mapping(translation()) -> string().
mapping(T) 	-> T#translation.mapping.

-spec func(translation()) -> fun().
func(T) 	-> T#translation.func.

-ifdef(TEST).

parse_test() ->
	TranslationDataStruct = {
		translation,
		"mapping",
		fun(X) -> X*2 end
	},

	Translation = parse(TranslationDataStruct),

	?assertEqual("mapping", Translation#translation.mapping),
	F = Translation#translation.func,
	?assertEqual(4, F(2)),
	ok.


getter_test() ->
	Translation = #translation{
		mapping = "mapping",
		func = fun(X) -> X*2 end
	},

	?assertEqual("mapping", mapping(Translation)),

	Fun = func(Translation),
	?assertEqual(4, Fun(2)),
	ok.

-endif.