%%%%%%%%%%%%%%%%%%%%%%
%%%    GLOBAL	   %%%
%%%%%%%%%%%%%%%%%%%%%%


%% SYSTEM SETTINGS
-define(NODE, node()).

-define(DATA_PATH, "./data/"). %% Add the trailing "/"

-define(APPLICATION, wins).
-define(ENV(Key), application:get_env(?APPLICATION, Key, [])).
-define(ENV(Key, Default), application:get_env(?APPLICATION, Key, Default)).


-define(CMP_TIMEOUT, 300). %% seconds


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

