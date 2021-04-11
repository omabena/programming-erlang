-module(register_atom).

-export([start/2]).

start(AnAtom, Fun) ->
    case whereis(AnAtom) of
        AnAtom -> fail;
        undefined -> register(AnAtom, spawn(fun() -> Fun() end))
    end.




    

