%%%%%%%%%%%%%%%%%%%%%%
%%%    GLOBAL	   %%%
%%%%%%%%%%%%%%%%%%%%%%


%% SYSTEM SETTINGS
-define(NODE, node()).

-define(DATA_PATH, "./data/"). %% Add the trailing "/"

-define(APPLICATION, wins).
-define(ENV(Key), application:get_env(?APPLICATION, Key, [])).
-define(ENV(Key, Default), application:get_env(?APPLICATION, Key, Default)).


%% POOLER SETTINGS
-define(POOL_COUNT, 20).
-define(POOL_MAX_COUNT, 50).


%% COWBOY SETTINGS
-define(COWBOY_WINS_GW_ACCEPTORS, 20).
-define(COWBOY_WINS_GW_PORT, 2250).


-record(win, {
	bid_id,
	cmp,
	crid,
	timestamp,
	win_price
}).

-record(click, {
	bid_id,
	cmp,
	crid,
	timestamp
}).

