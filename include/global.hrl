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
	bidder,
	cmp,
	crid,
	exchange,
	timestamp
}).

-record(click, {
	bid_id,
	bidder,
	cmp,
	crid,
	exchange,
	timestamp
}).

-record(conversion, {
	bid_id,
	bidder,
	cmp,
	crid,
	exchange,
	timestamp
}).



%%%%%%%%%%%%%%%%%%%%%%
%%%    RABBITMQ    %%%
%%%%%%%%%%%%%%%%%%%%%%

-record(subscriber, {
	name,
	exchange,
	type,
	topic,
	func,
	pool_size,
	logging = false
}).

-record(publisher, {
	name,
	exchange,
	topic,
	pool_size,
	logging = false
}).

%% RABBITMQ PUBSUB SUBSCRIBERS AND PUBLISHERS
-define(RMQ_SUBSCRIBERS, [
	#subscriber{
		name = cmp_config,
		exchange = <<"campaigns">>,
		type = pubsub,
		topic = <<"config.general">>,
		logging = false,
		func = fun(P) -> wins_cmp:load_cmp_config(P) end,
		pool_size = 5}
]).
-define(RMQ_PUBLISHERS, [
	#publisher{
		name = wins,
		exchange = <<"wins">>,
		topic = <<"wins.{bidder_id}">>,
		logging = false,
		pool_size = 20},
	#publisher{
		name = imps,
		exchange = <<"imps">>,
		topic = <<"imps.{bidder_id}">>,
		logging = false,
		pool_size = 10},
	#publisher{
		name = clicks,
		exchange = <<"clicks">>,
		topic = <<"clicks.{bidder_id}">>,
		logging = false,
		pool_size = 10}
]).
