-module(cuttlefish_translation).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

-record(translation, {
	mapping::string(),
	func::fun()
	}).

-export([
	parse/1, 
	mapping/1, 
	func/1]).

parse({translation, Mapping, Fun}) ->
	#translation{
		mapping = Mapping,
		func = Fun
	}.

mapping(T) 	-> T#translation.mapping.
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