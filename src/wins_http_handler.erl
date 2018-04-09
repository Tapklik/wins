-module(wins_http_handler).

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
			   Cmp = cowboy_req:binding(cmp, Req),
			   Crid = cowboy_req:binding(crid, Req),

			   QsVals = cowboy_req:parse_qs(Req),
			   Test = proplists:get_value(<<"test">>, QsVals, <<"0">>),
			   Exchange = proplists:get_value(<<"x">>, QsVals, <<"1">>),

			   WinNotification1 = #win{
				   bid_id = proplists:get_value(<<"b">>, QsVals, undefined),
				   cmp = Cmp,
				   crid = Crid,
				   timestamp = binary_to_integer(proplists:get_value(<<"ts">>, QsVals, 0)),
				   win_price = proplists:get_value(<<"wp">>, QsVals, 0.0),
				   exchange = Exchange
			   },

			   WinNotification2 = wins_decrypt:decrypt(WinNotification1, Exchange),

			   case check_valid_win(WinNotification2) of
				   valid when Test == <<"0">> ->
					   case wins_server:log_win(WinNotification2) of
						   {ok, _} ->
							   "Success";
						   _ ->
							   statsderl:increment(<<"wins.error">>, 1, 1.0),
							   "Error: invalid call"
					   end;
				   valid when Test == <<"1">> ->
					   ?INFO("WINS SERVER (TEST): Win -> [timestamp: ~p,  cmp: ~p,  crid: ~p,  win_price: ~p,  exchange: ~p,  bid_id: ~p",
						   [
							   WinNotification2#win.timestamp,
							   WinNotification2#win.cmp,
							   WinNotification2#win.crid,
							   WinNotification2#win.win_price,
							   WinNotification2#win.exchange,
							   WinNotification2#win.bid_id
						   ]),
					   "Success";
				   {invalid, Error} ->
					   statsderl:increment(<<"wins.error">>, 1, 1.0),
					   ?ERROR("WINS SERVER: Win notifications error [Req: ~p]. (Error: ~p)", [Req, Error]),
					   "Error: invalid call"
			   end
		   catch
			   _:E ->
				   statsderl:increment(<<"wins.error">>, 1, 1.0),
				   ?ERROR("WINS SERVER: Win notifications error [Req: ~p]. (Error: ~p)", [Req, E]),
				   "Error: invalid call"
		   end,
	{Resp, Req, State}.

%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%

check_valid_win(#win{bid_id = undefined}) ->
	{invalid, <<"invalid bid_id">>};
check_valid_win(#win{cmp = undefined}) ->
	{invalid, <<"invalid cmp">>};
check_valid_win(#win{crid = undefined}) ->
	{invalid, <<"invalid crid">>};
check_valid_win(#win{timestamp = 0}) ->
	{invalid, <<"invalid timestamp">>};
check_valid_win(#win{win_price  = 0.0}) ->
	{invalid, <<"invalid win price">>};
check_valid_win(#win{win_price = <<"key_integrity_error">>}) ->
	{invalid, <<"key_integrity_error">>};
check_valid_win(#win{}) ->
	valid;
check_valid_win(_) ->
	{invalid, <<"invalid format">>}.