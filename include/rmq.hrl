%%%%%%%%%%%%%%%%%%%%%%
%%%    GLOBAL	   %%%
%%%%%%%%%%%%%%%%%%%%%%

-record(subscriber, {
	name,
	exchange,
	topic,
	func,
	pool_size,
	logging
}).

-record(publisher, {
	name,
	exchange,
	topic,
	pool_size,
	logging
}).

%% RABBITMQ PUBSUB SETTINGS
-define(RMQ_HOST, os:getenv("RMQ_HOST", "104.225.218.109")).
-define(RMQ_PORT, 5672).
-define(RMQ_USER, <<"tapklik">>).
-define(RMQ_PASSWORD, <<"tapKlik7-rabbitmq">>).
-define(RMQ_VHOST, <<"/erl">>).


%% RABBITMQ PUBSUB SUBSCRIBERS AND PUBLISHERS
-define(RMQ_SUBSCRIBERS, [
	#subscriber{
		name = cmp_config,
		exchange = <<"campaigns">>,
		topic = <<"config.general">>,
		logging = true,
		func = fun(P) -> wins_creatives:load_cmp_config(P) end,
		pool_size = 5}
]).
-define(RMQ_PUBLISHERS, [
	#publisher{
		name = wins,
		exchange = <<"wins">>,
		topic = <<"wins.wins">>,
		logging = true,
		pool_size = 50},
	#publisher{
		name = imps,
		exchange = <<"wins">>,
		topic = <<"wins.imps">>,
		logging = true,
		pool_size = 50},
	#publisher{
		name = clicks,
		exchange = <<"wins">>,
		topic = <<"wins.clicks">>,
		logging = true,
		pool_size = 20}
]).
