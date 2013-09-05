-module(cuttlefish_message_handler).

-define(STDOUT(Str, Args), io:format(Str ++ "~n", Args)).
-define(STDERR(Str, Args), io:format(standard_error, Str ++ "~n", Args)).

%% "log" levels
-export([error/2, warning/2, check/1]).

%% @doc for pre1, this module is simply a lager pass through.
%% for pre2, it will be a gen_server that stores a collection of messages
%% at each phase of processing, cuttlefish will check this gen_server for 
%% errors. If there are any, show them all and exit.
%%
%% This way, if you, for example, have two datatype violations, it won't
%% fail on the first one, it will print out *ALL* of those violations.

-behaviour(gen_server).

-export([start_link/0]).

%% gen_server api
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-record(message_handler, {
       errors = [] :: [{string(), list()}],
       warnings = [] :: [{string(), list()}]
    }).

%% Public API
error(String, Args) ->
    lager:error(String, Args),
    gen_server:cast(?MODULE, {error, lists:flatten(io_lib:format(String, Args))}).

warning(String, Args) ->
    lager:warning(String, Args),
    gen_server:cast(?MODULE, {warning, lists:flatten(io_lib:format(String, Args))}).

check(Phase) ->
    case gen_server:call(?MODULE, {check, Phase}) of
        ok -> ok;
        _ExitReason -> 
            ?STDERR("Exiting", []),
            halt(1)
    end.

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% gen_server callbacks
init(_Args) -> {ok, #message_handler{}}.

handle_call({check, Phase}, _From, State) ->
    Reply = case length(State#message_handler.errors) of
        0 -> ok;
        _ ->
            lager:error("Error during ~p phase, aborting", [Phase]),
            error
    end,
    {reply, Reply, #message_handler{}}.
 
handle_cast({error, Message}, State) ->
    {noreply, State#message_handler{errors = [Message|State#message_handler.errors]}};
handle_cast({warning, Message}, State) ->
    {noreply, State#message_handler{errors = [Message|State#message_handler.warnings]}}.

handle_info(Msg, State) ->
    ?STDERR("~p", [Msg]),
    {noreply, State}.

terminate(normal, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    %% No change planned. The function is there for the behaviour,
    %% but will not be used. Only a version on the next
    {ok, State}.