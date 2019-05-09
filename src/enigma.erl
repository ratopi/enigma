%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2019, Ralf Thomas Pietsch
%%% @doc
%%%
%%% @end
%%% Created : 09. May 2019 21:48
%%%-------------------------------------------------------------------
-module(enigma).
-author("Ralf Thomas Pietsch <ratopi@abwesend.de>").

-record(enigma, {walzen, ukw, output}).

%% API
-export([init/2, encode/2, finish/1]).
-compile(export_all).



init(Walzen, Ukw) ->
	#enigma{walzen = Walzen, ukw = Ukw, output = []}.



encode([], Engima) when is_record(Engima, enigma) ->
	Engima;

encode([Char | T], Engima) when is_record(Engima, enigma) ->
	NewEnigma = encode(Char, Engima),
	encode(T, NewEnigma);

encode(32, Enigma) when is_record(Enigma, enigma) ->
	Enigma;

encode(Char, Enigma) when Char >= $a, Char =< $z, is_record(Enigma, enigma) ->
	encode(Char - $a + $A, Enigma);

encode(Char, Enigma) when Char >= $A, Char =< $Z, is_record(Enigma, enigma) ->
	EncodedChar = encode_inner(Char, Enigma#enigma.walzen, Enigma#enigma.ukw),
	NewWalzen = rotate_walzen(Enigma#enigma.walzen),
	Enigma#enigma{walzen = NewWalzen, output = [EncodedChar | Enigma#enigma.output]}.



finish(Enigma) when is_record(Enigma, enigma) ->
	lists:reverse(Enigma#enigma.output).


% ---


rotate_walzen([]) ->
	[];

rotate_walzen([W | T]) ->
	case enigma_walze:step(W) of
		{NewW, carry} ->
			[NewW | rotate_walzen(T)];
		{NewW, _} ->
			[NewW | T]
	end.



encode_inner(Char, Walzen, Ukw) ->
	Char2 = encode_inner_walzen(Char, Walzen, forward),
	Char3 = enigma_walze:encode(Char2, Ukw, forward),
	encode_inner_walzen(Char3, lists:reverse(Walzen), backward).



encode_inner_walzen(Char, [], _) ->
	Char;

encode_inner_walzen(Char, [W | T], Direction) ->
	NewChar = enigma_walze:encode(Char, W, Direction),
	encode_inner_walzen(NewChar, T, Direction).
