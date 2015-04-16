-module(bgs_app).

-behaviour(application).
-behaviour(supervisor).

-include("bgs.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

-export([start/0, stop/0, test/2]).
-export([start/2, stop/1]).
-export([init/1]).

start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

start(_Type, _Args) ->
	?DEBUG("Start BGS~n", []),
    supervisor:start_link({local, example_sup}, ?MODULE, []).

stop(_State) ->
    ok.

init([]) ->
    Pools = [
		{pool1, [
			{size, 10},
			{max_overflow, 20}
		], [test]},
		{pool2, [
			{size, 5},
			{max_overflow, 10}
		], [test]}
	],
    PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
        PoolArgs = [{name, {local, Name}},
                    {worker_module, bgs_worker}] ++ SizeArgs,
        poolboy:child_spec(Name, PoolArgs, WorkerArgs)
    end, Pools),
    {ok, {{one_for_one, 10, 10}, PoolSpecs}}.
	
test(PoolName, Msg) ->
    poolboy:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, {test, Msg})
    end).