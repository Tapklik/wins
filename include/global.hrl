%%%%%%%%%%%%%%%%%%%%%%
%%%    GLOBAL	   %%%
%%%%%%%%%%%%%%%%%%%%%%


%% SYSTEM SETTINGS
-define(NODE, node()).

-define(DATA_PATH, "./data/"). %% Add the trailing "/"

-define(APPLICATION, wins).
-define(ENV(Key), application:get_env(?APPLICATION, Key, [])).
-define(ENV(Key, Default), application:get_env(?APPLICATION, Key, Default)).

-define(RMQ_HOST, ?ENV(rmq_host, "localhost")).
-define(STREAM_WINS_TOPIC, ?ENV(stream_wins_topic, <<"Wins">>)).


%% POOLER SETTINGS
-define(POOL_COUNT, 200).
-define(POOL_MAX_COUNT, 350).


%% COWBOY SETTINGS
-define(COWBOY_WINS_GW_ACCEPTORS, 500).
-define(COWBOY_WINS_GW_PORT, 2250).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%    AD EXCHANGES SETTINGS	   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% GOOGLE
-define(KEY_ENCRYPTION, <<"e67TPztIo66lHRzPSWwAGE+Wm0LPmUjZ3LD0dGOUwkw=">>).
-define(KEY_INTEGRITY, <<"1EVzH0zXwn1mNsm9jieIvzLCOS7Z56+tEUxyd2Sfnyc=">>).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%    RECORDS & TYPES	   %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-record(opts, {
	test = false,
	clicktag = <<"">>,
	redirect = <<"">>
}).

-record(win, {
	bid_id,
	bidder,
	cmp,
	crid,
	timestamp,
	exchange,
	win_price
}).


-record(imp, {
	bid_id,
	cmp,
	crid,
	exchange,
	timestamp
}).

-record(click, {
	bid_id,
	cmp,
	crid,
	exchange,
	timestamp
}).

-record(conversion, {
	bid_id,
	cmp,
	crid,
	exchange,
	timestamp
}).

