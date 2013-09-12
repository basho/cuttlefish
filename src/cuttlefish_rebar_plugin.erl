%% -------------------------------------------------------------------
%%
%% cuttlefish_rebar_plugin: generates an application's default .conf
%% as part of the build
%%
%% Copyright (c) 2013 Basho Technologies, Inc.
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

-module(cuttlefish_rebar_plugin).

-export([
    generate/2
]).

%% ===================================================================
%% Public API
%% ===================================================================
generate(Config0, ReltoolFile) ->
    %% Load the reltool configuration from the file
    {Config, ReltoolConfig} = rebar_rel_utils:load_config(Config0, ReltoolFile),

    case should_i_run(ReltoolConfig) of
        ok ->
            TargetDir = rebar_rel_utils:get_target_dir(Config, ReltoolConfig),

            %% Finally, overlay the files specified by the overlay section
            case lists:keyfind(overlay, 1, ReltoolConfig) of
                {overlay, Overlays} when is_list(Overlays) ->
                    SchemaOverlays = lists:filter(fun(Overlay) -> 
                            element(1, Overlay) =:= template 
                                andalso filename:extension(element(3, Overlay)) =:= ".schema"
                        end, 
                        Overlays), 

                    Schemas = lists:sort(fun(A,B) -> filename:basename(A) > filename:basename(B) end, [
                        lists:flatten(filename:join(TargetDir, element(3, Schema)))
                    || Schema <- SchemaOverlays]),

                    io:format("Schema: ~p~n", [Schemas]),

                    {_Translations, Mappings, _Validators} = cuttlefish_schema:files(Schemas),
                    
                    %% TODO: output file configurable
                    Filename = filename:join([TargetDir, "etc", "riak.conf"]),
                    
                    cuttlefish_conf:generate_file(Mappings, Filename),
                    ok;
                false ->
                    %%io:format("No {overlay, [...]} found in reltool.config.\n", []);
                    ok;
                _ ->
                    io:format("{overlay, [...]} entry in reltool.config "
                           "must be a list.\n", [])
            end,
            ok;
        no ->
            ok
    end,
    ok.

%% Only run for top level project
should_i_run(ReltoolConfig) ->
    case lists:keyfind(sys, 1, ReltoolConfig) of
        {sys, _} = _SysTuple ->
            ok;
        false ->
            no
    end.
