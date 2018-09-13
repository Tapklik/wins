%%%%%%%%%%%%%%%%%%%%%%
%%%    GLOBAL	   %%%
%%%%%%%%%%%%%%%%%%%%%%

-record(subscriber, {
	name,
	exchange,
	type,
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
		pool_size = 50},
	#publisher{
		name = imps,
		exchange = <<"imps">>,
		topic = <<"imps.{bidder_id}">>,
		logging = false,
		pool_size = 50},
	#publisher{
		name = clicks,
		exchange = <<"clicks">>,
		topic = <<"clicks.{bidder_id}">>,
		logging = false,
		pool_size = 10}
]).
