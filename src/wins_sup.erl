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
	Children = [PoolerSup, RmqSup, VMServer],
	RestartStrategy = {one_for_one, 10, 300},
	{ok, {RestartStrategy, Children}}.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%
