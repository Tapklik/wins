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
			   QsVals = cowboy_req:parse_qs(Req),
			   WinNotification = #win{
				   bid_id = proplists:get_value(<<"bidid">>, QsVals, undefined),
				   cmp = proplists:get_value(<<"c">>, QsVals, undefined),
				   crid = proplists:get_value(<<"cr">>, QsVals, undefined),
				   timestamp = binary_to_integer(proplists:get_value(<<"ts">>, QsVals, 0)),
				   win_price = binary_to_float(proplists:get_value(<<"wp">>, QsVals, 0.0))
				   },
			   case check_valid_win(WinNotification) of
				   valid ->
					   case wins_server:log_win(WinNotification) of
						   {ok, _} ->
							   "Success";
						   _ ->
							   "Error: invalid call"
					   end;
				   {invalid, Error} ->
					   ?ERROR("WINS SERVER: Win notifications error [Req: ~p]. (Error: ~p)", [Req, Error]),
					   "Error: invalid call"
			   end
		   catch
			   _:E ->
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
check_valid_win(#win{}) ->
	valid;
check_valid_win(_) ->
	{invalid, <<"invalid format">>}.