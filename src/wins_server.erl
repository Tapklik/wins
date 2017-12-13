-module(wins_server).

-behaviour(gen_server).

-include("wins_global.hrl").
-include("lager.hrl").

-export([start_link/0]).
-export([log_win/1, log_win_click/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-record(state, {
	call_ref
}).

-record(brod_produce_reply, {
	call_ref,
	result
}).

%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
	?INFO("WINS: Started Wins Server client (Pid: ~p)", [self()]),
	gen_server:start_link(?MODULE, [], []).


log_win(WinNotification) ->
	case try_get_worker() of
		{ok, Worker} ->
			gen_server:call(Worker, {log_win, WinNotification});
		E -> E
	end.

log_win_click(WinNotification) ->
	case try_get_worker() of
		{ok, Worker} ->
			gen_server:call(Worker, {log_win_click, WinNotification});
		E -> E
	end.

%%%%%%%%%%%%%%%%%%%%%%
%%%    CALLBACKS   %%%
%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
	process_flag(trap_exit, true),
	?INFO("WINS SERVER: Win notifications service (pid: ~p) started.", [self()]),
	{ok, #state{}}.

handle_call({log_win, #win{
	bid_id = BidId, cmp = Cmp, crid = Crid, timestamp = TimeStamp, win_price = WinPrice
}}, _From, State) -> tk_lib:echo1(winprice, WinPrice),
	AdjustedWinPrice = WinPrice * 1000,
	Data = #{
		<<"timestamp">> => TimeStamp,    		% time stamp (5 mins)
		<<"bid_id">> => BidId,          		% id
		<<"cmp">> => Cmp,                		% campaign id
		<<"crid">> => Crid,                		% creative id
		<<"win_price">> => AdjustedWinPrice 	% win price
	},
	?INFO("WINS SERVER: Win -> [timestamp: ~p,  cmp: ~p,  crid: ~p,  win_price: $~p,  bid_id: ~p",
		[TimeStamp, Cmp, Crid, WinPrice, BidId]),
	rmq:publish(wins, term_to_binary(Data)),
	{reply, {ok, successful}, State};

handle_call({log_win_click, #click{
	bid_id = BidId, cmp = Cmp, crid = Crid, timestamp = TimeStamp
}}, _From, State) ->
	Data = #{
		<<"timestamp">> => TimeStamp,    		% time stamp (5 mins)
		<<"bid_id">> => BidId,          		% id
		<<"cmp">> => Cmp,                		% campaign id
		<<"crid">> => Crid                		% creative id
	},
	?INFO("WINS SERVER: Click -> [timestamp: ~p,  cmp: ~p,  crid: ~p,  bid_id: ~p",
		[TimeStamp, Cmp, Crid, BidId]),
	rmq:publish(clicks, term_to_binary(Data)),
	{reply, {ok, successful}, State};

handle_call(_Request, _From, State) ->
	{reply, ok, State}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_info(#brod_produce_reply{call_ref = _CallRef, result = brod_produce_req_acked}, State) ->
	{noreply, State};
handle_info({'EXIT', _, _}, State) ->
	{stop, shutdown, State}.

terminate(shutdown, _State) ->
	?ERROR("WINS SERVER: Win notifications service (pid: ~p) stopped.", [self()]),
	ok;
terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%

try_get_worker() ->
	try_get_worker(3).
try_get_worker(0) ->
	{error, no_members_available};
try_get_worker(N) ->
	case pooler:take_member(wins_pool) of
		error_no_members ->
			?WARN("POOLER (~p): No members available! Retrying [1/3]... ", [wins_pool]),
			try_get_worker(N - 1);
		W ->
			{ok, W}
	end.