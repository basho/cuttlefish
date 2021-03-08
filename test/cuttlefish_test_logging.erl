-module(cuttlefish_test_logging).

-behaviour(gen_event).

-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/0, add_handler/0]).
-export([set_up/0, log/2, reset/0, get_logs/0, bounce/0, bounce/1]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    logs :: [string() | binary()
]}).

%%
%% API
%%

set_up() ->
    Pid = case start_link() of
        {ok, Pid0}                       -> Pid0;
        {error, {already_started, Pid1}} -> Pid1
    end,
    case lists:member(?MODULE, gen_event:which_handlers(Pid)) of
        true -> ok;
        false ->
            gen_event:add_handler(Pid, ?MODULE, [])
    end,
    ok.

-spec get_logs() -> [iolist()] | {error, term()}.
get_logs() ->
    gen_event:call(?SERVER, ?MODULE, get_logs, infinity).

bounce() ->
    bounce(error).

bounce(Level) ->
    gen_event:call(?SERVER, ?MODULE, reset),
    logger:remove_handler(?MODULE),
    logger:add_handler(?SERVER, ?MODULE, #{
        level => Level
    }),
    ok.

start_link() ->
    gen_event:start_link({local, ?SERVER}).

log(LogEvent, Config) ->
    gen_event:call(?SERVER, ?MODULE, {log, LogEvent, Config}).

reset() ->
    gen_event:call(?SERVER, ?MODULE, reset).


add_handler() ->
    gen_event:add_handler(?SERVER, ?MODULE, []).

%%
%% Callbacks
%%

init([]) ->
    {ok, #state{

        logs = []
    }}.

handle_event(Event, State = #state{logs = Logs}) ->
    {ok, State#state{logs = [Event | Logs]}};
handle_event(_Event, State) ->
    {ok, State}.

handle_call({log, LogEvent, LogConfig}, State = #state{logs = Logs0}) ->
    #{formatter := {FModule, FConfig}} = LogConfig,
    %% [Time, " ", LevelStr, Message]
    Log = FModule:format(LogEvent, FConfig),
    Logs = [Log | Logs0],
    {ok, Logs, State#state{logs = Logs}};
handle_call(get_logs, State = #state{logs = Logs}) ->
    {ok, lists:reverse(Logs), State};
handle_call(reset, State) ->
    Reply = ok,
    {ok, Reply, State#state{logs = []}};
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.


handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
