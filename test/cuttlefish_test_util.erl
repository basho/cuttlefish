%% -------------------------------------------------------------------
%%
%% Copyright (c) 2017 Basho Technologies, Inc.
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

%%
%% @doc Common functions for use in tests.
%%
-module(cuttlefish_test_util).

% Public API
-export([
    fixtures_dir/0,
    fixtures_file/1,
    priv_dir/0,
    priv_file/1,
    test_dir/0,
    test_file/1
]).

-include_lib("eunit/include/eunit.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

-spec fixtures_dir() -> file:filename().
%%
%% @doc Returns the path to the "test_fixtures" directory.
%%
%% Rebar versions 2 and 3 differ on where the "test_fixtures" directory is
%% relative to the current test directory.
%%
fixtures_dir() ->
    Key = {?MODULE, fixtures_dir},
    case erlang:get(Key) of
        undefined ->
            {ok, CWD} = file:get_cwd(),
            Here = filename:join(CWD, "test_fixtures"),
            Dir = case filelib:is_dir(Here) of
                true ->
                    Here;
                _ ->
                    UpOne = filename:join(filename:dirname(CWD), "test_fixtures"),
                    ?assertEqual(true, filelib:is_dir(UpOne)),
                    UpOne
            end,
            _ = erlang:put(Key, Dir),
            Dir;
        Val ->
            Val
    end.

-spec fixtures_file(File :: file:filename()) -> file:filename().
%%
%% @doc Returns the path to a file in the "test_fixtures" directory.
%%
%% `File' is assumed to be a simple (possibly relative) filename under the
%% directory returned by {@fixtures_dir/0}.
%%
fixtures_file(File) ->
    filename:join(fixtures_dir(), File).

-spec priv_dir() -> file:filename().
%%
%% @doc Returns the path to the current "priv" directory.
%%
%% Rebar versions 2 and 3 differ on where the "priv" directory is relative
%% to the current test directory.
%%
priv_dir() ->
    Key = {?MODULE, priv_dir},
    case erlang:get(Key) of
        undefined ->
            TestPeer = filename:join(filename:dirname(test_dir()), "priv"),
            Dir = case filelib:is_dir(TestPeer) of
                true ->
                    TestPeer;
                _ ->
                    {ok, CWD} = file:get_cwd(),
                    Here = filename:join(CWD, "priv"),
                    case filelib:is_dir(Here) of
                        true ->
                            Here;
                        _ ->
                            UpOne = filename:join(filename:dirname(CWD), "priv"),
                            ?assertEqual(true, filelib:is_dir(UpOne)),
                            UpOne
                    end
            end,
            _ = erlang:put(Key, Dir),
            Dir;
        Val ->
            Val
    end.

-spec priv_file(File :: file:filename()) -> file:filename().
%%
%% @doc Returns the path to a file in the current "priv" directory.
%%
%% `File' is assumed to be a simple (possibly relative) filename under the
%% directory returned by {@link priv_dir/0}.
%%
priv_file(File) ->
    filename:join(priv_dir(), File).

-spec test_dir() -> file:filename().
%%
%% @doc Returns the path to the current "test" directory.
%%
%% Rebar versions 2 and 3 differ widely on how and where files are laid out
%% when running eunit, but when in the "test" directory both place the beam
%% file in the same directory as its source.
%%
test_dir() ->
    Key = {?MODULE, test_dir},
    case erlang:get(Key) of
        undefined ->
            Dir = filename:dirname(code:which(?MODULE)),
            _ = erlang:put(Key, Dir),
            Dir;
        Val ->
            Val
    end.

-spec test_file(File :: file:filename()) -> file:filename().
%%
%% @doc Returns the path to a file in the current "test" directory.
%%
%% `File' is assumed to be a simple (possibly relative) filename under the
%% directory returned by {@link test_dir/0}.
%%
test_file(File) ->
    filename:join(test_dir(), File).

