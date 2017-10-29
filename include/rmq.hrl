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
-define(RMQ_HOST, os:getenv("RMQ_HOST", "de-c1-srv-01")).
-define(RMQ_PORT, 5672).
-define(RMQ_USER, <<"tapklik">>).
-define(RMQ_PASSWORD, <<"tapKlik7-rabbitmq">>).
-define(RMQ_VHOST, <<"/erl">>).


%% RABBITMQ PUBSUB SUBSCRIBERS AND PUBLISHERS
-define(RMQ_SUBSCRIBERS, [

]).
-define(RMQ_PUBLISHERS, [
	#publisher{
		name = wins,
		exchange = <<"wins">>,
		topic = <<"wins.imps">>,
		logging = true,
		pool_size = 10},
	#publisher{
		name = clicks,
		exchange = <<"wins">>,
		topic = <<"wins.clicks">>,
		logging = true,
		pool_size = 4}
]).
