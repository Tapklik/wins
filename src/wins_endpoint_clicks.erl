-module(wins_endpoint_clicks).

-include("global.hrl").
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
	Cmp = cowboy_req:binding(cmp, Req),
	Crid = cowboy_req:binding(crid, Req),
	QsVals = cowboy_req:parse_qs(Req),
	Test = is_test(proplists:get_value(<<"test">>, QsVals)),
	Redirect = proplists:get_value(<<"r">>, QsVals, <<"">>),
	Click = #click{
		cmp = Cmp,
		crid = Crid,
		bid_id = proplists:get_value(<<"b">>, QsVals, undefined),
		bidder = proplists:get_value(<<"adr">>, QsVals),
		timestamp = binary_to_integer(proplists:get_value(<<"ts">>, QsVals, <<"0">>)),
		exchange = proplists:get_value(<<"x">>, QsVals, <<"1">>)
	},
	case check_valid_click(Click) of
		valid ->
			case wins_server:log_click(Click, [{test, Test}, {redirect, Redirect}]) of
				{ok, null} ->
					statsderl:increment(<<"clicks.error">>, 1, 1.0),
					?ERROR("WINS SERVER: Click notifications error [Req: ~p]. (Error: No ctrurl set!!)", [Req]),
					"Error: invalid call";
				{ok, R} ->
					cowboy_req:reply(302, #{
						<<"Location">> => R
					}, Req);
				_ ->
					statsderl:increment(<<"clicks.error">>, 1, 1.0),
					"Error: invalid call"
			end;
		{invalid, Error} ->
			statsderl:increment(<<"clicks.error">>, 1, 1.0),
			?ERROR("WINS SERVER: Click notifications error [Req: ~p]. (Error: ~p)", [Req, Error]),
			"Error: invalid call"
	end,
	%% todo add cases for ets and solve cache busting problem with 301 code
	{stop, Req, State}.

%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%

is_test(<<"1">>) ->
	true;
is_test(_) ->
	false.

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