-module(cuttlefish_sup).

-behavior(supervisor). 

%% API
-export([start_link/0]). 

%% Supervisor callbacks
-export([init/1]). 

-define(SERVER, ?MODULE).
-spec start_link() -> {ok, pid()} | ignore | {error, any()}.
start_link() ->
          supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec init(list()) -> {ok, {tuple(), list()}} | ignore | {error, any()}.
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,
    AChild = {'MessageHander', {cuttlefish_message_handler, start_link, []},
              Restart, Shutdown, Type, ['cuttlefish_message_handler']},
    {ok, {SupFlags, [AChild]}}.