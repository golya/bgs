-module(game_server).
 
-behaviour(gen_server).
 
-export([start_link/0, set_socket/2]).
 
-export([init/1, handle_call/3,
         handle_cast/2, handle_info/2, terminate/2, code_change/3]).
 
 
-record(state, {
                socket,    % client socket
                addr       % client address
               }).
 
-define(TIMEOUT, 120000).
 
%%%------------------------------------------------------------------------
%%% API
%%%------------------------------------------------------------------------
 
%%-------------------------------------------------------------------------
%% @spec (Socket) -> {ok,Pid} | ignore | {error,Error}
%% @doc To be called by the supervisor in order to start the server.
%%      If init/1 fails with Reason, the function returns {error,Reason}.
%%      If init/1 returns {stop,Reason} or ignore, the process is
%%      terminated and the function returns {error,Reason} or ignore,
%%      respectively.
%% @end
%%-------------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).
 
set_socket(Pid, Socket) when is_pid(Pid), is_port(Socket) ->
    gen_server:call(Pid, {socket_ready, Socket}).
 
%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------
 
%%-------------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%% @private
%%-------------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.
 
handle_call({socket_ready, Socket}, _From, State) ->
	inet:setopts(Socket, [{active, once}]),
	{ok, {IP, Port}} = inet:peername(Socket),
	io:format("Client socket is ready: [~p, ~p] ~p", [IP, Port, Socket]),
	{reply, ok, State#state{socket=Socket, addr=IP}, ?TIMEOUT};
handle_call(_Msg, _From, StateData) ->
	io:format("msg [~p]~n", [_Msg]),
	{reply, ok, StateData}.

handle_cast(_Msg, StateData) ->
	{noreply, StateData}.

 
%%-------------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%% @private
%%-------------------------------------------------------------------------

handle_info({tcp, _Socket, Msg}, #state{socket=S} = StateData) ->
	inet:setopts(S, [{active, once}]),
	gen_tcp:send(S, Msg),
	%% io:format("message: ~p ~p ~p~n", [Msg, S, P]),
    {noreply, StateData};
handle_info({tcp_closed, _}, StateData) ->
    {stop, normal, StateData};
handle_info(Info, StateData) ->
    {stop, Info, StateData}.
 
%%-------------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%% @private
%%-------------------------------------------------------------------------
terminate(_Reason, #state{socket=Socket}) ->
    (catch gen_tcp:close(Socket)),
    ok.
 
%%-------------------------------------------------------------------------
%% Func: code_change/4
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState, NewStateData}
%% @private
%%-------------------------------------------------------------------------
code_change(_OldVsn, StateData, _Extra) ->
    {ok, StateData}.