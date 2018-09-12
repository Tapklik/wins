%%%-------------------------------------------------------------------
%%% @author adi
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. Apr 2018 11:20 AM
%%%-------------------------------------------------------------------
-module(wins_endpoint_imps).
-author("adi").

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
	Type = cowboy_req:binding(type, Req),
	Cmp = cowboy_req:binding(cmp, Req),
	Crid = cowboy_req:binding(crid, Req),
	QsVals = cowboy_req:parse_qs(Req),
	Test = is_test(proplists:get_value(<<"test">>, QsVals)),
	Imp = #imp{
		cmp = Cmp,
		crid = Crid,
		bid_id = proplists:get_value(<<"b">>, QsVals, undefined),
		bidder = proplists:get_value(<<"adr">>, QsVals),
		timestamp = binary_to_integer(proplists:get_value(<<"ts">>, QsVals, <<"0">>)),
		exchange = proplists:get_value(<<"x">>, QsVals, <<"1">>)
	},
	Resp = case check_valid_imp(Imp) of
			   valid when Type == <<"h">> ->
				   ClickTag = proplists:get_value(<<"clickTag">>, QsVals, <<"">>),
				   case wins_server:log_imp(Imp, [{clicktag, ClickTag}, {test, Test}]) of
					   {ok, Ad} ->
						   cowboy_req:reply(200, #{<<"content-type">> => <<"text/html">>}, Ad, Req),
						   stop;
					   {error, Error} ->
						   statsderl:increment(<<"imps.error">>, 1, 1.0),
						   ?ERROR("WINS SERVER: Imp notifications error [Req: ~p]. (Error: ~p)", [Req, Error]),
						   "Error: invalid call"
				   end;
			   valid ->
				   case wins_server:log_imp(Imp, [{test, Test}]) of
					   {ok, _} ->
						   <<"">>;
					   {error, Error} ->
						   statsderl:increment(<<"imps.error">>, 1, 1.0),
						   ?ERROR("WINS SERVER: Imp notifications error [Req: ~p]. (Error: ~p)", [Req, Error]),
						   "Error: invalid call"
				   end;
			   {invalid, Error} ->
				   statsderl:increment(<<"imps.error">>, 1, 1.0),
				   ?ERROR("WINS SERVER: Imp notifications error [Req: ~p]. (Error: ~p)", [Req, Error]),
				   "Error: invalid call"
		   end,
	cowboy_req:set_resp_headers(#{<<"Content-Type">> => <<"text/html">>}, Req),
	{Resp, Req, State}.

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

check_valid_imp(#imp{bid_id = undefined}) ->
	{invalid, <<"invalid bid_id">>};
check_valid_imp(#imp{cmp = undefined}) ->
	{invalid, <<"invalid cmp">>};
check_valid_imp(#imp{crid = undefined}) ->
	{invalid, <<"invalid crid">>};
check_valid_imp(#imp{timestamp = 0}) ->
	{invalid, <<"invalid timestamp">>};
check_valid_imp(#imp{}) ->
	valid;
check_valid_imp(_) ->
	{invalid, <<"invalid format">>}.

