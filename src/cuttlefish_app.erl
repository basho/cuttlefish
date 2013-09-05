-module(cuttlefish_app).
-behavior(application). 

%% Application callbacks
-export([start/2, stop/1]).

-spec start(_StartType, list()) -> {ok, pid()} | any().
start(_StartType, _StartArgs) ->
    case cuttlefish_sup:start_link() of 
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error 
    end.

-spec stop(_State) -> ok.
stop(_State) ->
    ok.
