-module(wins_db).

-behaviour(gen_server).

-include("wins_global.hrl").
-include("rmq.hrl").
-include("lager.hrl").


-export([start_link/0, insert/1, get/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).


-record(state, {
	hour
}).

-define(INTERVAL, 5000).


%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
	gen_server:start_link(?MODULE, [], []).

insert(#win{
	cmp = Cmp,
	crid = Crid,
	timestamp = Ts,
	exchange = Exchange,
	win_price = WinPrice}) ->
	Key = {Cmp, Crid, Ts, Exchange},
	case ets:member(wins_db, Key) of
		false ->
			ets:insert(wins_db, {Key, time_server:get_datehour(), 0, 0, 0, 0, 0});
		true -> ok
	end,
	ets:update_counter(wins_db, Key, {3, WinPrice}),
	ets:update_counter(wins_db, Key, {4, 1});
insert(#click{
	cmp = Cmp,
	crid = Crid,
	timestamp = Ts,
	exchange = Exchange}) ->
	Key = {Cmp, Crid, Ts, Exchange},
	ets:update_counter(wins_db, Key, {6, 1}).

get(_Qs) ->
	%% TODO add Qs parameters and checking
	ReportId = get_report_id(),
	PrivDir = code:priv_dir(wins),
	File = PrivDir ++ "/reports/" ++ binary_to_list(ReportId),
	spawn(fun() -> write_ets_to_file(File, ReportId) end),
	{ok, ReportId}.


%%%%%%%%%%%%%%%%%%%%%%
%%%    CALLBACKS   %%%
%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
	process_flag(trap_exit, true),
	ets:new(wins_db, [public, named_table, {write_concurrency, true}]),
	%% Init
	erlang:send_after(?INTERVAL, self(), {interval}),
	{ok, #state{}}.


handle_call(_Request, _From, State) ->
	{reply, ok, State}.


handle_cast(_Request, State) ->
	{noreply, State}.


handle_info({interval}, State) ->
	DateHour1 = time_server:get_datehour(),
	case State#state.hour of
		undefined -> ok;
		DateHour0 when DateHour0 == DateHour1 -> ok;
		DateHour0 ->
			{Y0, M0, D0, H0} = DateHour0,
			{Y1, M1, D1} = edate:shift({Y0, M0, D0}, -2, days),
			DateHourToDelete = {Y1, M1, D1, H0},
			spawn(
				fun() ->
					ets:match_delete(wins_db, {'$1', DateHourToDelete, '_', '_', '_', '_', '_'})
				end
			)
	end,
	erlang:send_after(?INTERVAL, self(), {interval}),
	{noreply, State#state{hour = DateHour1}};
handle_info({stop}, State) ->
	{stop, shutdown, State};
handle_info({'EXIT', _, _}, State) ->
	{stop, shutdown, State}.


terminate(_Reason, _State) ->
	ok.


code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%

get_report_id() ->
	{T1, T2, _T3} = erlang:timestamp(),
	Time = (T1 * 1000000) + T2,
	Uid = integer_to_binary(Time),
	<<"report_", Uid/binary, ".csv">>.


write_ets_to_file(File, ReportId) ->
	case ets:match_object(wins_db, '$1') of
		[] ->
			?WARN("WINS REPORTS: No data for report ~p", [ReportId]),
			ok;
		RawData ->
			T1 = erlang:monotonic_time(),
			{ok, IO} = file:open(File, [write]),
			lists:foreach(
				fun(Data) ->
					{
						{Cmp, Crid, Ts0, Exchange},
						{Y0, M0, D0, H0},
						WinPrice0, Wins0, Imps0, Clicks0, Conversions0
					} = Data,
					Ts1 = integer_to_binary(Ts0),
					Y1 = integer_to_binary(Y0),
					M1 = integer_to_binary(M0),
					D1 = integer_to_binary(D0),
					H1 = integer_to_binary(H0),
					WinPrice1 = integer_to_binary(WinPrice0),
					Wins1 = integer_to_binary(Wins0),
					Imps1 = integer_to_binary(Imps0),
					Clicks1 = integer_to_binary(Clicks0),
					Conversions1 = integer_to_binary(Conversions0),
					DataBin = <<
						Cmp/binary, ",",
						Crid/binary, ",",
						Ts1/binary, ",",
						Exchange/binary, ",",
						Y1/binary, ",",
						M1/binary, ",",
						D1/binary, ",",
						H1/binary, ",",
						WinPrice1/binary, ",",
						Wins1/binary, ",",
						Imps1/binary, ",",
						Clicks1/binary, ",",
						Conversions1/binary, "\n"
					>>,
					file:write(IO, DataBin)
				end
			, RawData),
			file:close(IO),
			?INFO("WINS REPORTS: Report ~p is ready! (Process time: ~p ms)", [ReportId, calc_time(T1)])
	end.


%% @hidden
calc_time(T1) ->
	%% STAT: Calculate bid response time
	T2 = erlang:monotonic_time(),
	erlang:convert_time_unit(T2 - T1, native, milli_seconds).