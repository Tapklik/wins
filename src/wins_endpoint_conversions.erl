%%%-------------------------------------------------------------------
%%% @author adi
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Apr 2018 11:22 AM
%%%-------------------------------------------------------------------
-module(wins_endpoint_conversions).
-author("adi").

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
			   Exchange = proplists:get_value(<<"x">>, QsVals, <<"1">>),

			   ConversionNotification = #conversion{
				   bid_id = proplists:get_value(<<"b">>, QsVals, undefined),
				   cmp = proplists:get_value(<<"c">>, QsVals, undefined),
				   crid = proplists:get_value(<<"cr">>, QsVals, undefined),
				   timestamp = binary_to_integer(proplists:get_value(<<"ts">>, QsVals, 0)),
				   exchange = Exchange
			   },
			   case check_valid_conversion(ConversionNotification) of
				   valid when Test == <<"0">> ->
					   case wins_server:log_conversion(ConversionNotification) of
						   {ok, _} ->
							   "Success";
						   _ ->
							   statsderl:increment(<<"conversions.error">>, 1, 1.0),
							   "Error: invalid call"
					   end;
				   valid when Test == <<"1">> ->
					   ?INFO("WINS SERVER (TEST): Conversion -> [timestamp: ~p,  cmp: ~p,  crid: ~p,  exchange: ~p,  bid_id: ~p",
						   [
							   ConversionNotification#click.timestamp,
							   ConversionNotification#click.cmp,
							   ConversionNotification#click.crid,
							   ConversionNotification#click.exchange,
							   ConversionNotification#click.bid_id
						   ]),
					   "Success";
				   {invalid, Error} ->
					   statsderl:increment(<<"conversions.error">>, 1, 1.0),
					   ?ERROR("WINS SERVER: Conversion notifications error [Req: ~p]. (Error: ~p)", [Req, Error]),
					   "Error: invalid call"
			   end
		   catch
			   _:E ->
				   statsderl:increment(<<"conversions.error">>, 1, 1.0),
				   ?ERROR("WINS SERVER: Conversion notifications error [Req: ~p]. (Error: ~p)", [Req, E]),
				   "Error: invalid call"
		   end,
	{Resp, Req, State}.

%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%

check_valid_conversion(#conversion{bid_id = undefined}) ->
	{invalid, <<"invalid bid_id">>};
check_valid_conversion(#conversion{cmp = undefined}) ->
	{invalid, <<"invalid cmp">>};
check_valid_conversion(#conversion{crid = undefined}) ->
	{invalid, <<"invalid crid">>};
check_valid_conversion(#conversion{timestamp = 0}) ->
	{invalid, <<"invalid timestamp">>};
check_valid_conversion(#conversion{}) ->
	valid;
check_valid_conversion(_) ->
	{invalid, <<"invalid format">>}.

