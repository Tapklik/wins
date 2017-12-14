-module(wins_clicks_http_handler).

-include("wins_global.hrl").
-include("lager.hrl").

-export([
	init/2,
	allowed_methods/2,
	content_types_provided/2
]).

-export([handle_get/2]).



init(Req, Opts) ->
	{cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
	{[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
	{[
		{<<"application/json">>, handle_get}
	], Req, State}.

handle_get(Req, State) ->
	Resp = try
			   QsVals = cowboy_req:parse_qs(Req),
			   Test = proplists:get_value(<<"test">>, QsVals, <<"0">>),
			   ClickNotification = #click{
				   bid_id = proplists:get_value(<<"bidid">>, QsVals, undefined),
				   cmp = proplists:get_value(<<"c">>, QsVals, undefined),
				   crid = proplists:get_value(<<"cr">>, QsVals, undefined),
				   timestamp = binary_to_integer(proplists:get_value(<<"ts">>, QsVals, 0))
			   },
			   case check_valid_click(ClickNotification) of
				   valid when Test == <<"0">> ->
					   case wins_server:log_win_click(ClickNotification) of
						   {ok, _} ->
							   "Success";
						   _ ->
							   "Error: invalid call"
					   end;
				   valid when Test == <<"1">> ->
					   ?INFO("WINS SERVER (TEST): Click -> [timestamp: ~p,  cmp: ~p,  crid: ~p,  exchange: ~p,  bid_id: ~p",
						   [
							   ClickNotification#click.timestamp,
							   ClickNotification#click.cmp,
							   ClickNotification#click.crid,
							   ClickNotification#click.exchange,
							   ClickNotification#click.bid_id
						   ]),
					   "Success";
				   {invalid, Error} ->
					   ?ERROR("WINS SERVER: Click notifications error [Req: ~p]. (Error: ~p)", [Req, Error]),
					   "Error: invalid call"
			   end
		   catch
			   _:E ->
				   ?ERROR("WINS SERVER: Click notifications error [Req: ~p]. (Error: ~p)", [Req, E]),
				   "Error: invalid call"
		   end,
	{Resp, Req, State}.

%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%

check_valid_click(#click{bid_id = undefined}) ->
	{invalid, <<"invalid bid_id">>};
check_valid_click(#click{cmp = undefined}) ->
	{invalid, <<"invalid cmp">>};
check_valid_click(#click{crid = undefined}) ->
	{invalid, <<"invalid crid">>};
check_valid_click(#click{timestamp = 0}) ->
	{invalid, <<"invalid timestamp">>};
check_valid_click(#click{}) ->
	valid;
check_valid_click(_) ->
	{invalid, <<"invalid format">>}.