-module(wins_server).

-behaviour(gen_server).

-include("global.hrl").
-include("lager.hrl").

-export([start_link/0]).
-export([log_win/2, log_imp/2, log_click/2, log_conversion/2]).

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


log_win(WinNotification, Opts) ->
	case try_get_worker() of
		{ok, Worker} ->
			gen_server:call(Worker, {log_win, WinNotification, parse_opts(Opts)});
		E -> E
	end.

log_imp(Imp, Opts) ->
	case try_get_worker() of
		{ok, Worker} ->
			gen_server:call(Worker, {log_imp, Imp, parse_opts(Opts)});
		E -> E
	end.

log_click(Click, Opts) ->
	case try_get_worker() of
		{ok, Worker} ->
			gen_server:call(Worker, {log_click, Click, parse_opts(Opts)});
		E -> E
	end.

log_conversion(Conversion, Opts) ->
	%% TODO
	ok.

%%%%%%%%%%%%%%%%%%%%%%
%%%    CALLBACKS   %%%
%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
	process_flag(trap_exit, true),
	?INFO("WINS SERVER: Win notifications service (pid: ~p) started.", [self()]),
	{ok, #state{}}.

handle_call({log_win, #win{
	bid_id = BidId, bidder = Bidder, cmp = Cmp, crid = Crid, timestamp = TimeStamp, exchange = Exchange, win_price = WinPrice
}, Opts}, _From, State) ->
	{ok, AccId} = wins_cmp:get_cmp_account(Cmp),
	Spend = case wins_cmp:get_cmp_fees(Cmp) of
				{ok, Fees} ->
					VariableFees = tk_maps:get([<<"variable">>], Fees),
					FixedFees = tk_maps:get([<<"fixed">>], Fees),
					trunc(WinPrice + (VariableFees / 100 * WinPrice) + FixedFees);
				_ -> WinPrice
			end,

	Data = #{
		<<"action">> => <<"win">>,
		<<"timestamp">> => TimeStamp,           % time stamp (5 mins)
		<<"bid_id">> => BidId,                    % id
		<<"acc">> => AccId,                 % account id
		<<"cmp">> => Cmp,                        % campaign id
		<<"crid">> => Crid,                    % creative id
		<<"exchange">> => Exchange,            % exchange
		<<"win_price">> => WinPrice,            % win price (CPI)
		<<"spend">> => Spend                    % spend (CPI)
	},
	?INFO("WINS SERVER: Win -> [timestamp: ~p,  cmp: ~p,  crid: ~p,  price: (buy: $~p / sell: $~p),  exchange: ~p,  bidder: ~p, bid_id: ~p",
		[TimeStamp, Cmp, Crid, WinPrice, Spend, Exchange, Bidder, BidId]),
	rmq:publish(wins, <<"wins.", Bidder/binary>>, term_to_binary(Data)),
	log_internal(wins, Data, Opts),
	pooler:return_member(wins_pool, self()),
	{reply, {ok, successful}, State};

handle_call({log_imp, #imp{
	bid_id = BidId, cmp = Cmp, crid = Crid, timestamp = TimeStamp, exchange = Exchange
}, Opts}, _From, State) ->
	{ok, AccId} = wins_cmp:get_cmp_account(Cmp),
	Data = #{
		<<"action">> => <<"impression">>,
		<<"timestamp">> => TimeStamp,           % time stamp (5 mins)
		<<"bid_id">> => BidId,                  % id
		<<"acc">> => AccId,                 % account id
		<<"cmp">> => Cmp,                       % campaign id
		<<"crid">> => Crid,                     % creative id
		<<"exchange">> => Exchange,             % exchange
		<<"win_price">> => 0,                    % win price (CPI)
		<<"spend">> => 0                        % spend (CPI)
	},
	?INFO("WINS SERVER: Imp -> [timestamp: ~p,  cmp: ~p,  crid: ~p,  exchange: ~p,  bid_id: ~p",
		[TimeStamp, Cmp, Crid, Exchange, BidId]),
	log_internal(imps, Data, Opts),
	[{_, CreativeMap} | _] = ets:lookup(creatives, {Cmp, Crid}),
	Ad = case tk_maps:get([<<"class">>], CreativeMap) of
			 <<"html5">> ->
				 Html0 = tk_maps:get([<<"html">>], CreativeMap),
				 H = integer_to_binary(tk_maps:get([<<"h">>], CreativeMap)),
				 W = integer_to_binary(tk_maps:get([<<"w">>], CreativeMap)),
				 ClickTaq = tk_lib:escape_uri(Opts#opts.clicktag),
				 Html1 = <<Html0/binary, "?ct=", ClickTaq/binary>>,
				 <<"<iframe src='", Html1/binary, "' marginwidth='0' marginheight='0' align='top' scrolling='no' frameborder='0'"
					 , "hspace='0' vspace='0' height='", H/binary, "' width='", W/binary, "'></iframe>">>;
			 <<"banner">> ->
				 tk_maps:get([<<"path">>], CreativeMap);
			 _ ->
				 ?ERROR("WINS SERVER: Bad creative type [cmp: ~p,  crid: ~p]", [Cmp, Crid]),
				 ok
		 end,
	pooler:return_member(wins_pool, self()),
	{reply, {ok, Ad}, State};

handle_call({log_click, #click{
	bid_id = BidId, cmp = Cmp, crid = Crid, timestamp = TimeStamp, exchange = Exchange
} = Click, Opts}, _From, State) ->
	{ok, AccId} = wins_cmp:get_cmp_account(Cmp),
	Data = #{
		<<"action">> => <<"click">>,
		<<"timestamp">> => TimeStamp,           % time stamp (5 mins)
		<<"bid_id">> => BidId,                  % id
		<<"acc">> => AccId,                    % account id
		<<"cmp">> => Cmp,                       % campaign id
		<<"crid">> => Crid,                     % creative id
		<<"exchange">> => Exchange,             % exchange
		<<"win_price">> => 0,                    % win price (CPI)
		<<"spend">> => 0                        % spend (CPI)
	},
	log_internal(clicks, Data, Opts),
	[{_, CreativeMap} | _] = ets:lookup(creatives, {Cmp, Crid}),
	Redirect = tk_maps:get([<<"ctrurl">>], CreativeMap),
	AdditionalRediret = Opts#opts.redirect,
	spawn(fun() ->
		relay_click_to_adx(AdditionalRediret, Click)
		  end),
	?INFO("WINS SERVER: Click -> [timestamp: ~p,  cmp: ~p,  crid: ~p,  exchange: ~p,  bid_id: ~p] [CTR: ~p, Redirect#1: ~p]",
		[TimeStamp, Cmp, Crid, Exchange, BidId, Redirect, AdditionalRediret]),
	% TODO add second redirect
	pooler:return_member(wins_pool, self()),
	{reply, {ok, Redirect}, State};

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


log_internal(_, _, #opts{test = true}) ->
	ok;
log_internal(Topic, #{<<"bid_id">> := BidId} = Data, #opts{test = false}) ->
	TopicBin = atom_to_binary(Topic, latin1),
	statsderl:increment(<<TopicBin/binary, ".total">>, 1, 1.0),
	wins_db:insert(Topic, Data),
	publish_to_stream(?STREAM_WINS_TOPIC, BidId, Data).


publish_to_stream(Topic, BidId, Load0) ->
	spawn(
		fun() ->
			case ?ENV(stream_enabled) of
				true ->
					Load = base64:encode(jsx:encode(Load0)),
					kinetic:put_record([
						{<<"Data">>, Load},
						{<<"PartitionKey">>, BidId},
						{<<"StreamName">>, Topic}
					]);
				_ ->
					ok
			end
		end).


relay_click_to_adx(AdxRedirectLink, Click) when is_binary(AdxRedirectLink) ->
	relay_click_to_adx(binary_to_list(AdxRedirectLink), Click);
relay_click_to_adx(AdxRedirectLink, #click{
	bid_id = BidId, cmp = Cmp, crid = Crid, timestamp = TimeStamp, exchange = Exchange
}) ->
	case AdxRedirectLink of
		"" ->
			?WARN("WINS SERVER: AdX redirect link empty! (timestamp: ~p,  cmp: ~p,  crid: ~p,  exchange: ~p,  bid_id: ~p)",
				[TimeStamp, Cmp, Crid, Exchange, BidId]);
		_ ->
			case httpc:request(get, {AdxRedirectLink, []}, [], []) of
				{ok, _} ->
					ok;
				_ ->
					?ERROR("WINS SERVER: Error relaying click to Ad exchange! (timestamp: ~p,  cmp: ~p,  crid: ~p,  exchange: ~p,  bid_id: ~p)",
						[TimeStamp, Cmp, Crid, Exchange, BidId])
			end
	end.


parse_opts([]) ->
	#opts{};
parse_opts(Opts) ->
	parse_opts(Opts, #opts{}).
parse_opts([], R) ->
	R;
parse_opts([{test, Test} | T], R) ->
	parse_opts(T, R#opts{test = Test});
parse_opts([{redirect, Redirect} | T], R) ->
	parse_opts(T, R#opts{redirect = Redirect});
parse_opts([{clicktag, ClickTag} | T], R) ->
	parse_opts(T, R#opts{clicktag = ClickTag}).