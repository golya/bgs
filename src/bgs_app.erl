-module(bgs_app).

-behaviour(application).
-behaviour(supervisor).

-include("bgs.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================
	
-compile(export_all).

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
	 
start_server(Port) ->
	Pid = spawn_link(fun() ->
		{ok, Listen} = gen_tcp:listen(Port, [binary, {active, false}]),
		spawn(fun() -> acceptor(Listen) end),
		timer:sleep(infinity)
	end),
	{ok, Pid}.
 
acceptor(ListenSocket) ->
	{ok, Socket} = gen_tcp:accept(ListenSocket),
	spawn(fun() -> acceptor(ListenSocket) end),
	handle(Socket).
	
ip_address(Socket) ->
    case inet:peername(Socket) of
        {ok, {Ip, Port}} ->
            io:format("ip ~p, port ~p~n", [Ip, Port]);
        {error, Error} ->
            io:format("error ~p~n", [Error])
    end.
	

-define(PORTNO, 8889).

start_link() ->
    start_link(?PORTNO).
start_link(P) ->
    spawn_link(?MODULE, loop0, [P]).

loop0(Port) ->
    case gen_tcp:listen(Port, [binary, {reuseaddr, true},
			       {packet, 0}, {active, false}]) of
	{ok, LSock} ->
		io:format("spawn worker ~p~n", [LSock]),	
	    spawn(?MODULE, worker, [self(), LSock]),
	    loop(LSock);
	Other ->
	    io:format("Can't listen to socket ~p~n", [Other])
    end.


loop(S) ->
    receive
		next_worker ->
			io:format("next_worker ~p~n", [S]),
			spawn_link(?MODULE, worker, [self(), S])
    end,
    loop(S).
	

worker(Server, LS) ->
    case gen_tcp:accept(LS) of
	{ok, Socket} ->
	    Server ! next_worker,
		io:format("Socket ~p~n", [Socket]),
	    handle(Socket);
	{error, Reason} ->
	    Server ! next_worker,
	    io:format("Can't accept socket ~p~n", [Reason])
    end.

	 
handle(Socket) ->
	inet:setopts(Socket, [{active, once}]),
	receive
		{tcp, Socket, <<"quit", _/binary>>} ->
			gen_tcp:close(Socket);
		{tcp, Socket, <<"join", _/binary>>} ->
			io:format("~p, ~s~n", [Socket, "the client want to join"]),
			ip_address(Socket),
			gen_tcp:send(Socket, "its accepted"),
			handle(Socket);
		{tcp, Socket, Msg} ->
			io:format("~p, ~s~n", ["message is: ", Msg]),
			ip_address(Socket),
			gen_tcp:send(Socket, Msg),
			handle(Socket)
	end.
	

even(X) when X >= 0 -> 
	(X band 1) == 0.
odd(X) when X > 0 -> 
	not even(X).