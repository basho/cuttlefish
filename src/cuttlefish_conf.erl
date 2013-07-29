-module(cuttlefish_conf).

-export([generate/1]).

generate(Schema) ->
	lists:foldl(
		fun(SchemaElement, ConfFile) ->
			ConfFile ++ generate_element(SchemaElement)
		end, [], Schema).

generate_element(SchemaElement) ->
	{Key, Default, Attributes} = SchemaElement,
	Commented = proplists:get_value(commented, Attributes),
	Advanced = proplists:get_value(advanced, Attributes),
	IncDef = proplists:get_value(include_default, Attributes),  
	%% @advanced: leave out of generated .conf file
	%% @commeneted $val: insert into .conf file, but commented out with $val
	%% @include_default $val:  substitute '$name' or whatever in the key for $val
	%%    e.g. @include_default internal
	%%         listener.http.$name -> listener.http.internal 

	Field = cuttlefish_util:variable_key_replace(Key, IncDef),

	case generate_element(Advanced, Default, Commented) of
		no ->
			[];
		commented ->
			Comments = generate_comments(SchemaElement),
			Comments ++ [lists:flatten([ "## ", Field, " = ", Commented ]), ""];
		default ->
			Comments = generate_comments(SchemaElement),
			Comments ++ [lists:flatten([ Field, " = ", Default ]), ""]	
	end.

generate_element(true, _, _) -> no;
generate_element(_, undefined, undefined) -> no;
generate_element(_, _Default, undefined) -> default;
generate_element(_, undefined, _Comment) -> commented;
generate_element(_Advanced, _Default, _Commented) -> no.

generate_comments(SchemaElement) ->
	{_Field, _Default, Attributes} = SchemaElement,
	Doc = proplists:get_value(doc, Attributes, []),
	[ "## " ++ D || D <- Doc].

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).
-endif.

-ifdef(TEST).

generate_element_test() ->

	TestSchemaElement =
        {"ring_size", undefined, 
            [
             {datatype,{integer,[]}},
             {mapping, "riak_core.ring_creation_size"},
             {commented, "64"},
             {doc, ["Default ring creation size.  Make sure it is a power of 2,",
				    "e.g. 16, 32, 64, 128, 256, 512 etc"]}]
        },

	GeneratedConf = generate_element(TestSchemaElement),

	?assertEqual(4, length(GeneratedConf)),
	?assertEqual(
		"## Default ring creation size.  Make sure it is a power of 2,",
		lists:nth(1, GeneratedConf)
		), 
	?assertEqual(
		"## e.g. 16, 32, 64, 128, 256, 512 etc",
		lists:nth(2, GeneratedConf)
		), 
	?assertEqual(
		"## ring_size = 64",
		lists:nth(3, GeneratedConf)
		), 
	?assertEqual(
		"",
		lists:nth(4, GeneratedConf)
		), 
	ok.

generate_dollar_test() ->
	TestSchemaElement =         
		{ "listener.http.$name", "127.0.0.1:8098",
            [
             {datatype,{ip,[]}},
             {mapping, "riak_core.http"},
             {include_default,"internal"}
            ]},
   	_GeneratedConf = generate_element(TestSchemaElement),

   	ok.

generate_comments_test() ->
	SchemaElement = {"dont.care", undefined, [
		{doc, ["Hi!", "Bye!"]}
	]},
	Comments = generate_comments(SchemaElement),
	?assertEqual(["## Hi!", "## Bye!"], Comments).

-endif.