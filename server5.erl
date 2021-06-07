-module(server5).

-export([rpc/2, start/0]).

start() -> spawn(fun () -> wait() end).

wait() -> receive {become, F} -> F() end.

rpc(Pid, Q) ->
    Pid ! {self(), Q},
    receive {Pid, Reply} -> Reply end.
