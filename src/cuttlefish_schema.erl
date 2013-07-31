%% -------------------------------------------------------------------
%%
%% cuttlefish_schema: slurps schema files
%%
%% Copyright (c) 2013 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(cuttlefish_schema).

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

-spec string(string()) -> {[{string(), fun(), list()}], [{string(), any(), list()}]}.
string(S) -> 
    {ok, Tokens, _} = erl_scan:string(S),
    CommentTokens = erl_comment_scan:string(S),
    Schemas = parse_schema(Tokens, CommentTokens),
    lists:partition(fun cuttlefish_translation:is_translation/1, Schemas).

parse_schema(Tokens, Comments) ->
    parse_schema(Tokens, Comments, []).

parse_schema([], _, Acc) ->
    lists:reverse(Acc);
parse_schema(ScannedTokens, CommentTokens, Acc) ->
    {LineNo, Tokens, TailTokens } = parse_schema_tokens(ScannedTokens),
    {Comments, TailComments} = lists:foldr(
        fun(X={CommentLineNo, _, _, Comment}, {C, TC}) -> 
            case CommentLineNo < LineNo of
                true -> {Comment ++ C, TC};
                _ -> {C, [X|TC]}
            end
        end, 
        {[], []}, 
        CommentTokens),
    %%{ Key, Default } = parse(Tokens),
    Tuple = try parse(Tokens) of
        T -> T
    catch
        _:_ -> io:format("Error parsing ~p~n", [Tokens])
    end,
    Attributes = comment_parser(Comments),

    Item = case element(1, Tuple) of
        mapping ->
            {mapping, Key, Mapping, Proplist} = Tuple,
            Doc = proplists:get_value(doc, Attributes, []), 
            cuttlefish_mapping:parse({mapping, Key, Mapping, [{doc, Doc}|Proplist]});
        translation ->
            cuttlefish_translation:parse(Tuple)
    end,
    parse_schema(TailTokens, TailComments, [Item| Acc]).

parse_schema_tokens(Scanned) -> 
    parse_schema_tokens(Scanned, []).

parse_schema_tokens(Scanned, Acc=[{dot, LineNo}|_]) ->
    {LineNo, lists:reverse(Acc), Scanned};
parse_schema_tokens([H|Scanned], Acc) ->
    parse_schema_tokens(Scanned, [H|Acc]).

-spec parse(list()) -> {string(), any()}.
parse(Scanned) ->
    {ok,Parsed} = erl_parse:parse_exprs(Scanned),
    {value, X, _} = erl_eval:exprs(Parsed,[]),
    X.

comment_parser(Comments) ->
    StrippedComments = 
        lists:filter(fun(X) -> X =/= [] end, 
            [percent_stripper(C) || C <- Comments]),
    %% now, let's go annotation hunting

    AttrList = lists:foldl(
        fun(Line, Acc) ->
                case {Line, Acc} of
                    {[ $@ | T], _} ->
                        Annotation = hd(string:tokens(T, [$\s])),
                        [{list_to_atom(Annotation), [percent_stripper(T -- Annotation)] }|Acc];
                    { _, []} -> [];
                    {String, _} ->
                        [{Annotation, Strings}|T] = Acc,
                        [{Annotation, [String|Strings]}|T]
                end
            end, [], StrippedComments), 
    SortedList = lists:reverse([ {Attr, lists:reverse(Value)} || {Attr, Value} <- AttrList]),
    CorrectedList = attribute_formatter(SortedList),
    CorrectedList.

%% Just handles the @doc business
attribute_formatter([Other | T]) ->
    [ Other | attribute_formatter(T)];
attribute_formatter([]) -> [].

percent_stripper(Line) ->
    percent_stripper_r(percent_stripper_l(Line)).

percent_stripper_l([$%|T]) -> percent_stripper_l(T);
percent_stripper_l([$\s|T]) -> percent_stripper_l(T);
percent_stripper_l(Line) -> Line.

percent_stripper_r(Line) -> 
    lists:reverse(
        percent_stripper_l(
            lists:reverse(Line))).
-ifdef(TEST).

percent_stripper_test() ->
    ?assertEqual("hi!", percent_stripper("%%% hi!")),
    ?assertEqual("hi!", percent_stripper("%% hi!")),
    ?assertEqual("hi!", percent_stripper("% hi!")),
    ?assertEqual("hi!", percent_stripper(" hi!")),
    ?assertEqual("hi!", percent_stripper(" % % hi!")),
    ?assertEqual("hi!", percent_stripper("% % % hi!")),
    ?assertEqual("hi!", percent_stripper("% % % hi! % % %")),
    ok.

comment_parser_test() ->
    Comments = [
        " ",
        "%% @doc this is a sample doc",
        "%% it spans multiple lines %%",
        "",
        "%% there can be line breaks",
        "%% @datatype enum on, off",
        "%% @advanced",
        "%% @include_default name_substitution",
        "%% @mapping riak_kv.anti_entropy"
    ],
    ParsedComments = comment_parser(Comments),
    ?assertEqual(["this is a sample doc",
                  "it spans multiple lines",
                  "there can be line breaks"],
                  proplists:get_value(doc, ParsedComments)), 
    ok.

-endif.