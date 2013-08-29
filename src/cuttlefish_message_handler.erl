-module(cuttlefish_message_handler).

%% "log" levels
-export([error/2, warning/2]).

error(FormatStr, Args) ->
    lager:error(FormatStr, Args).

warning(FormatStr, Args) ->
    lager:warning(FormatStr, Args).

%% @doc for pre1, this module is simply a lager pass through.
%% for pre2, it will be a gen_server that stores a collection of messages
%% at each phase of processing, cuttlefish will check this gen_server for 
%% errors. If there are any, show them all and exit.
%%
%% This way, if you, for example, have two datatype violations, it won't
%% fail on the first one, it will print out *ALL* of those violations.

%%% -behaviour(gen_server).
%%% 
%%% -export([start_link/0]).
%%% 
%%% %% gen_server api
%%% -export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%%% 
%%% 
%%% -record(message_handler, {
%%%        errors = [] ::list()
%%%     }).
%%% 
%%% %% Public API
%%% error(String, Args) ->
%%%     gen_server:cast(?MODULE, {error, lists:flatten(io_lib:format(String, Args))}).
%%%     %%lager:error(String, Args).
%%% 
%%% start_link() -> gen_server:start_link(?MODULE, [], []).
%%% 
%%% %% gen_server callbacks
%%% init(_Args) -> {ok, #message_handler{}}.
%%% 
%%% handle_call(_Msg, From, State) ->
%%%     {reply, From, State}.
%%%  
%%% handle_cast({error, Message}, State) ->
%%%     {noreply, State#message_handler{errors = [Message|State#message_handler.errors]}}.
%%% 
%%% handle_info(_Msg, State) ->
%%%     {noreply, State}.
%%% 
%%% terminate(normal, _State) ->
%%%     ok.
%%% 
%%% code_change(_OldVsn, State, _Extra) ->
%%%     %% No change planned. The function is there for the behaviour,
%%%     %% but will not be used. Only a version on the next
%%%     {ok, State}.