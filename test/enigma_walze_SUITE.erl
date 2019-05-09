%%%-------------------------------------------------------------------
%%% @author Ralf Thomas Pietsch <ratopi@abwesend.de>
%%% @copyright (C) 2019, Ralf Thomas Pietsch
%%% @doc
%%%
%%% @end
%%% Created : 10. Mai 2019 00:20
%%%-------------------------------------------------------------------
-module(enigma_walze_SUITE).
-author("Ralf Thomas Pietsch <ratopi@abwesend.de>").

-include_lib("common_test/include/ct.hrl").

%% API
-export([init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2, all/0]).
-export([symetry/1, ukw/1]).

all() -> [
	symetry,
	ukw
].

% ---

init_per_suite(Config) ->
	Config.

end_per_suite(_Config) ->
	ok.

init_per_testcase(_, Config) ->
	Config.

end_per_testcase(_, _Config) ->
	ok.

% ---

symetry(_Config) ->
	symetry_test([{W, S, L} || W <- walzen(), S <- steps(), L <- letters()]).

ukw(_Config) ->
	W = enigma_walze:init_ukw(ukw_a, 1),
	{W, no_carry} = enigma_walze:step(W).

% ---

walzen() ->
	[i, ii, iii, iv, v, vi, vii, viii, ukw_a, ukw_b, ukw_c].

steps() ->
	lists:seq(1, 26).

letters() ->
	lists:seq($A, $Z).



symetry_test([]) ->
	ok;

symetry_test([{W, S, L} | T]) ->
	R = enigma_walze:encode(L, enigma_walze:init(W, S), forward),
	L = enigma_walze:encode(R, enigma_walze:init(W, S), backward),
	symetry_test(T).
