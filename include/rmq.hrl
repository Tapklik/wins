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
% Define RMQ_HOST in global.hrl
-define(RMQ_PORT, 5672).
-define(RMQ_USER, <<"tapklik">>).
-define(RMQ_PASSWORD, <<"tapKlik7-rabbitmq">>).
-define(RMQ_VHOST, <<"/erl">>).

-define(RMQ_X_MESSAGE_TTL, 60).


%% RABBITMQ PUBSUB SUBSCRIBERS AND PUBLISHERS
-define(RMQ_SUBSCRIBERS, [
	#subscriber{
		name = cmp_config,
		exchange = <<"campaigns">>,
		topic = <<"config.general">>,
		logging = true,
		func = fun(P) -> wins_cmp:load_cmp_config(P) end,
		pool_size = 5}
]).
-define(RMQ_PUBLISHERS, [
	#publisher{
		name = wins,
		exchange = <<"wins">>,
		topic = <<"wins.{bidder_id}">>,
		logging = false,
		pool_size = 50}
]).
