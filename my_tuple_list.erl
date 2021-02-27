-module(my_tuple_list).

-export([tuple_list/1]).

for(Max, Max, F) -> [F(Max)];
for(I, Max, F) -> [F(I) | for(I + 1, Max, F)].

tuple_list(B) ->
    for(1, tuple_size(B), fun (I) -> element(I, B) end).
