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
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


log_win(WinNotification) ->
	gen_server:call(?MODULE, {log_win, WinNotification}).

log_win_click(WinNotification) ->
	gen_server:call(?MODULE, {log_win_click, WinNotification}).

%%%%%%%%%%%%%%%%%%%%%%
%%%    CALLBACKS   %%%
%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
	process_flag(trap_exit, true),
	?INFO("WINS SERVER: Win notifications service (pid: ~p) started.", [self()]),
	{ok, #state{}}.

handle_call({log_win, #win{
	bid_id = BidId, cmp = Cmp, crid = Crid, timestamp = TimeStamp, win_price = WinPrice
}}, _From, State) ->
	AdjustedWinPrice = WinPrice * 1000,
	Data = #{
		<<"timestamp">> => TimeStamp,    		% time stamp (5 mins)
		<<"bid_id">> => BidId,          		% id
		<<"cmp">> => Cmp,                		% campaign id
		<<"crid">> => Crid,                		% creative id
		<<"win_price">> => AdjustedWinPrice 	% win price
	}, tk_lib:echo1(win, Data),
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

