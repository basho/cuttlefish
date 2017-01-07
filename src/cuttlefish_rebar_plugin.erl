%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013-2017 Basho Technologies, Inc.
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
%% @doc Rebar cuttlefish plugin to generate an application's default .conf
%%      file as part of the build.
%%
%% For now, this is specific to Rebar2, but it's been refactored to allow it
%% to be extended to work with Rebar3 as well.
%% When we decide how we want to use it, we should do that.
%%
-module(cuttlefish_rebar_plugin).

-export([
    generate/2,
    init/1
]).

%% ===================================================================
%% Rebar2 Plugin API
%% ===================================================================

%%
%% @doc Rebar2 plugin.
%%
generate(ConfigIn, ReltoolFile) ->
    case rebar_info() of
        {2, _, RelMod} = Inf ->
            case RelMod:is_rel_dir() of
                {true, _} ->
                    %% Load the reltool configuration from the file
                    {RebarConfig, ReltoolConfig} =
                        RelMod:load_config(ConfigIn, ReltoolFile),
                    rebar2run(Inf, RebarConfig, ReltoolConfig);
                _ ->
                    ok
            end;
        _ ->
            ok
    end.

%% ===================================================================
%% Rebar3 Plugin API
%% ===================================================================

%%
%% @doc Rebar3 provider initialization.
%%
init(State) ->
    {ok, State}.

%% ===================================================================
%% Internal
%% ===================================================================

rebar2run({_Vsn, CfgMod, RelMod}, RebarConfig, ReltoolConfig) ->
    TargetDir = RelMod:get_target_dir(RebarConfig, ReltoolConfig),
    %% Overlay the files specified by the overlay section
    case lists:keyfind(overlay, 1, ReltoolConfig) of
        {overlay, Overlays} when is_list(Overlays) ->
            SchemaOverlays = lists:filter(
                fun(Overlay) ->
                    element(1, Overlay) =:= template andalso
                        filename:extension(element(3, Overlay)) =:= ".schema"
                end, Overlays),

            Schemas = lists:sort([
                lists:flatten(filename:join(TargetDir, element(3, Schema)))
                || Schema <- SchemaOverlays]),

            io:format("Schema: ~p~n", [Schemas]),

            case cuttlefish_schema:files(Schemas) of
                {errorlist, _Es} ->
                    %% These errors were already printed
                    error;
                {_Translations, Mappings, _Validators} ->
                    %% I really wanted this to default to the application name.
                    %% The problem is that the type of application that uses
                    %% cuttlefish is also the kind that doesn't have an
                    %% .app.src file, so rebar doesn't get it.
                    %% I could have done something with cwd, but I didn't like
                    %% that because you could be building anywhere.
                    %% So, cuttlefish it is. he's pretty cool anyway.
                    File = CfgMod:get_local(
                        RebarConfig, cuttlefish_filename, "cuttlefish.conf"),
                    Filename = filename:join([TargetDir, "etc", File]),
                    cuttlefish_conf:generate_file(Mappings, Filename),
                    ok
            end;
        false ->
            io:put_chars("No {overlay, [...]} found in reltool.config.\n");
        _ ->
            io:put_chars(
                "{overlay, [...]} entry in reltool.config must be a list.\n")
    end.

-spec rebar_info() -> non_neg_integer() | tuple().
%
% Returns the version and, if the version's supported, the modules needed to
% access Rebar. ALL Rebar modules are returned as variables, as we don't want
% any dependencies on Rebar, nor do we want to listen to xref and dialyzer
% whining about unknown functions.
%
rebar_info() ->
    Vsn = case application:get_key(rebar, vsn) of
        {ok, [Ch | _] = VsnStr} when Ch >= $0 andalso Ch =< $9 ->
            {N, _} = string:to_integer(VsnStr),
            N;
        _ ->
            0
    end,
    case Vsn of
        2 ->
            {Vsn, rebar_config, rebar_rel_utils};
        3 ->
            {Vsn, renar_api, rebar_state, rebar_app_info};
        _ ->
            Vsn
    end.
