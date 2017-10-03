-module(wins).

-behaviour(application).

-include("wins_global.hrl").
-include("rmq.hrl").
-include("lager.hrl").


-export([start/0, start/2, stop/1]).


%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

start() ->
	_ = [application:start(Dep) || Dep <- resolve_deps(wins),
		not is_otp_base_app(Dep)],
	application:start(cowboy),
	RouteSpecs2 = [{"/sad23ref34578hj/wins", wins_http_handler, []}, {"/sad23ref34578hj/clicks", wins_clicks_http_handler, []}],
	Dispatch2 = cowboy_router:compile([
		{'_', RouteSpecs2}
	]),
	{ok, _} = cowboy:start_clear(http2, ?COWBOY_WINS_GW_ACCEPTORS, [{port, ?COWBOY_WINS_GW_PORT}], #{
		env => #{dispatch => Dispatch2}
	}),
	?INFO("COWBOY: Started ~p acceptors, on port ~p", [?COWBOY_WINS_GW_ACCEPTORS, ?COWBOY_WINS_GW_PORT]),
	%% Start RMQ Pub/Sub workers
	[rmq:start_subscriber(Subscriber) || Subscriber <- ?RMQ_SUBSCRIBERS],
	[rmq:start_publisher(Publisher) || Publisher <- ?RMQ_PUBLISHERS],
	ok.

start(_Type, _Args) ->
	wins_sup:start_link().

stop(_State) ->
	ok.


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%

dep_apps(App) ->
	application:load(App),
	{ok, Apps} = application:get_key(App, applications),
	Apps.

all_deps(App, Deps) ->
	[[all_deps(Dep, [App | Deps]) || Dep <- dep_apps(App),
		not lists:member(Dep, Deps)], App].

resolve_deps(App) ->
	DepList = all_deps(App, []),
	{AppOrder, _} = lists:foldl(fun(A, {List, Set}) ->
		case sets:is_element(A, Set) of
			true ->
				{List, Set};
			false ->
				{List ++ [A], sets:add_element(A, Set)}
		end
								end,
		{[], sets:new()},
		lists:flatten(DepList)),
	AppOrder.

is_otp_base_app(kernel) ->
	true;
is_otp_base_app(stdlib) ->
	true;
is_otp_base_app(_) ->
	false.
