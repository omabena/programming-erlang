-module(math_functions).

-export([even/1, filter/2, split/1]).

even(Number) ->
    case Number rem 2 of
        0 -> true;
        _ -> false
    end.

filter(F, L) -> filter_helper(F, L, []).

filter_helper(F, [H | T], Acc) ->
    case F(H) of
        true -> filter_helper(F, T, [H | Acc]);
        false -> filter_helper(F, T, Acc)
    end;
filter_helper(_F, [], Acc) -> lists:reverse(Acc).

split(L) -> even_odds(L, [], []).

even_odds([H | T], Even, Odds) ->
    case H rem 2 of
        0 -> even_odds(T, [H | Even], Odds);
        _ -> even_odds(T, Even, [H | Odds])
    end;
even_odds([], Even, Odds) ->
    {lists:reverse(Even), lists:reverse(Odds)}.
