-module(wins_decrypt).

-include("wins_global.hrl").
-include("lager.hrl").
-include_lib("eunit/include/eunit.hrl").


-export([decrypt/2]).

-ifdef(TEST).
-compile(export_all).
-endif.


%%%%%%%%%%%%%%%%%%%%%%
%%%    API CALLS   %%%
%%%%%%%%%%%%%%%%%%%%%%

decrypt(WinNotification, Exchange) ->
	internal_decrypt(WinNotification, Exchange).


%%%%%%%%%%%%%%%%%%%%%%
%%%    INTERNAL    %%%
%%%%%%%%%%%%%%%%%%%%%%

internal_decrypt(#win{win_price = WinPrice} = Win, <<"1">>) ->
	Win#win{win_price = WinPrice};

internal_decrypt(#win{win_price = WinPrice} = Win, <<"3">>) ->
	Win#win{win_price = google_price_decrypt(WinPrice)}.


google_price_decrypt(WinPrice) ->
	%% Key_Encryption and Key_Integrity from Google
	Ke0 = ?KEY_ENCRYPTION,
	Ki0 = ?KEY_INTEGRITY,
	Ke1 = base64url:decode(Ke0),
	Ki1 = base64url:decode(Ki0),

	<<InitVector:16/binary, EncryptedPrice:8/binary, Sig:4/binary>> = base64url:decode(WinPrice),

	%% get price pad with hmac -- limit to 8 bytes
	PricePad = crypto:hmac(sha, Ke1, InitVector, 8),

	%% get price through bxor
	PriceBin = wins_decrypt:bin_bxor(EncryptedPrice, PricePad),

	%% Verify: get sig with hmac -- limit to 4 bytes. Then compare to Sig in EncPrice
	ConfSig = crypto:hmac(sha, Ki1, <<PriceBin/binary, InitVector/binary>>, 4),
	case ConfSig == Sig of
		true ->
			%% price from google is in CPI (CPM $1.23 == CPI 1230)
			decode_binary_price(PriceBin) / 1000;
		false ->
			<<"key_integrity_error">>
	end.


bin_bxor(Bin1, Bin2) ->
	Sz1 = byte_size(Bin1)*8,
	Sz2 = byte_size(Bin2)*8,
	<<Int1:Sz1>> = Bin1,
	<<Int2:Sz2>> = Bin2,
	Int3 = Int1 bxor Int2,
	Sz3 = max(Sz1, Sz2),
	<<Int3:Sz3>>.


decode_binary_price(PriceBin) ->
	decode_binary_price(PriceBin, 0).
decode_binary_price(<<>>, Acc) -> Acc;
decode_binary_price(<<A:32, Rest/binary>>, Acc) ->
	decode_binary_price(Rest, Acc + A).


%%%%%%%%%%%%%%%%%%%
%%%    TESTS    %%%
%%%%%%%%%%%%%%%%%%%

decode_test_() ->
	[
		?_assert(decode_binary_price(<<0,0,0,0,0,0,0,100>>) == 100),
		?_assert(decode_binary_price(<<0,0,0,0,0,0,39,116>>) == 10100)
	].

decrypt_test_() ->
	[
		?_assert(google_price_decrypt(<<"WioYuAAOCXkKaY-OAAYHfxY9BS2OSx2Z4ydXTg">>) == 0.1),
		?_assert(google_price_decrypt(<<"WioYuAAOCYoKaY-OAAYHf6zb_YMMixvmmTMEtQ">>) == 3.1),
		?_assert(google_price_decrypt(<<"WioYuAAOCZgKaY-OAAYHf0sJg6ezegRE8SZagA">>) == 7.1),
		?_assert(google_price_decrypt(<<"WioYuAAOCaQKaY-OAAYHf0IygWWLIx5ngzuMqg">>) == 10.1)
	].
