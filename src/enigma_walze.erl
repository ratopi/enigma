%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2019, Ralf Thomas Pietsch
%%% @doc
%%%
%%% @end
%%% Created : 09. May 2019 21:32
%%%-------------------------------------------------------------------
-module(enigma_walze).
-author("Ralf Thomas Pietsch <ratopi@abwesend.de>").

-record(walze, {walze_id, walze, step, ukw = false}).

%% API
-export([init/2, init_ukw/2, encode/3, step/1]).
% -compile(export_all).


init(WalzenId, Step) ->
	Walze = walze_encoding(WalzenId),
	#walze{walze_id = WalzenId, walze = Walze, step = Step}.


init_ukw(WalzenId, Step) ->
	Walzen = walze_encoding(WalzenId),
	#walze{walze_id = WalzenId, walze = Walzen, step = Step, ukw = true}.


encode(Char, Walze, Direction) when is_record(Walze, walze), (Direction == forward orelse Direction == backward) ->
	W = rotate(Walze#walze.walze, Walze#walze.step),
	E = create_encoding(W),
	encode_inner(Char, E, Direction).



step(Walze = #walze{ukw = true}) ->
	{Walze, no_carry};

step(Walze = #walze{step = 26}) when is_record(Walze, walze) ->
	{Walze#walze{step = 1}, carry};

step(Walze) when is_record(Walze, walze) ->

	case Walze#walze.step >= 1 andalso Walze#walze.step =< 26 of

		true ->
			{Walze#walze{step = Walze#walze.step + 1}, no_carry};

		false ->
			erlang:error({illegal_step, Walze})

	end.


% ---

encode_inner(Char, [{Char, Encoded} | _], forward) ->
	Encoded;
encode_inner(Char, [{Encoded, Char} | _], backward) ->
	Encoded;
encode_inner(Char, [_ | T], Direction) ->
	encode_inner(Char, T, Direction).



rotate(Walze, 1) ->
	Walze;

rotate(Walze, Step) ->
	[H | T] = Walze,
	rotate(T ++ [H], Step - 1).



create_encoding(W) ->
	lists:zip(walze_encoding(ident), W).


walze_encoding(ident) -> "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
walze_encoding(shift1) -> "BCDEFGHIJKLMNOPQRSTUVWXYZA";

walze_encoding(i) -> "EKMFLGDQVZNTOWYHXUSPAIBRCJ";
walze_encoding(ii) -> "AJDKSIRUXBLHWTMCQGZNPYFVOE";
walze_encoding(iii) -> "BDFHJLCPRTXVZNYEIWGAKMUSQO";
walze_encoding(iv) -> "ESOVPZJAYQUIRHXLNFTGKDCMWB";
walze_encoding(v) -> "VZBRGITYUPSDNHLXAWMJQOFECK";
walze_encoding(vi) -> "JPGVOUMFYQBENHZRDKASXLICTW";
walze_encoding(vii) -> "NZJHGRCXMYSWBOUFAIVLPEKQDT";
walze_encoding(viii) -> "FKQHTLXOCBJSPDZRAMEWNIUYGV";

walze_encoding(ukw_a) -> "EJMZALYXVBWFCRQUONTSPIKHGD";
walze_encoding(ukw_b) -> "YRUHQSLDPXNGOKMIEBFZCWVJAT";
walze_encoding(ukw_c) -> "FVPJIAOYEDRZXWGCTKUQSBNMHL".
