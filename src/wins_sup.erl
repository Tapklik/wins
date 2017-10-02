-module(wins_sup).

-behaviour(supervisor).

-include("wins_global.hrl").
-include("lager.hrl").


-export([start_link/0]).
-export([init/1]).


%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%%%%%%%%%%%%%%%%%%%%%%
%%%    CALLBACKS   %%%
%%%%%%%%%%%%%%%%%%%%%%

init([]) ->
	BusGS = #{
		id => boss_gs,
		start => {boss_gs, start_link, []},
		restart => permanent,
		shutdown => infinity,
		type => worker,
		modules => [boss_gs]
	},
	PoolerSup = #{
		id => pooler_sup,
		start => {pooler_sup, start_link, []},
		restart => permanent,
		shutdown => infinity,
		type => supervisor,
		modules => [pooler_sup]
	},
	RmqSup = #{
		id => rmq_sup,
		start => {rmq_sup, start_link, []},
		restart => permanent,
		shutdown => infinity,
		type => supervisor,
		modules => [rmq_sup]
	},
	VMServer = #{
		id => vm,
		start => {vm, start_link, []},
		restart => permanent,
		shutdown => 2000,
		type => worker,
		modules => [vm]
	},
	CronSup = #{
		id => boss_cron_sup,
		start => {boss_cron_sup, start_link, []},
		restart => permanent,
		shutdown => infinity,
		type =>supervisor,
		modules => [boss_cron_sup]
	},
	CmpSup = #{
		id => boss_cmp_sup,
		start => {boss_cmp_sup, start_link, []},
		restart => permanent,
		shutdown => infinity,
		type =>supervisor,
		modules => [boss_cmp_sup]
	},
	WinsSup = #{
		id => boss_wins,
		start => {boss_wins, start_link, []},
		restart => permanent,
		shutdown => infinity,
		type => worker,
		modules => [boss_wins]
	},
	BidsDebugLogger = #{
		id => boss_bids_log,
		start => {boss_bids_log, start_link, []},
		restart => permanent,
		shutdown => 2000,
		type => worker,
		modules => [boss_bids_log]
	},
	%% Stats server gen_server
	StatsServer = #{
		id => boss_stats,
		start => {boss_stats, start_link, []},
		restart => permanent,
		shutdown => 2000,
		type => worker,
		modules => [boss_stats]
	},
	%% Time server gen_server
	TimeServer = #{
		id => time_server,
		start => {time_server, start_link, []},
		restart => permanent,
		shutdown => 2000,
		type => worker,
		modules => [time_server]
	},
	Children = [PoolerSup, CmpSup, CronSup, WinsSup, BusGS, RmqSup, StatsServer, TimeServer, VMServer, BidsDebugLogger],
	RestartStrategy = {one_for_one, 10, 300},
	{ok, {RestartStrategy, Children}}.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%
