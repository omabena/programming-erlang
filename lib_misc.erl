-module(lib_misc).

-export([consult/1,
         file_size_and_type/1,
         flush_buffer/0,
         for/3,
         keep_alive/2,
         ls/1,
         odds_and_even/1,
         on_exit/2,
         perms/1,
         priority_receive/0,
         pythag/1,
         qsort/1,
         sleep/1,
         string2value/1,
         unconsult/2]).

for(Max, Max, F) -> [F(Max)];
for(I, Max, F) -> [F(I) | for(I + 1, Max, F)].

qsort([]) -> [];
qsort([Pivot | T]) ->
    qsort([X || X <- T, X < Pivot]) ++
        [Pivot] ++ qsort([X || X <- T, X >= Pivot]).

pythag(N) ->
    [{A, B, C}
     || A <- lists:seq(1, N), B <- lists:seq(1, N),
        C <- lists:seq(1, N), A + B + C =< N,
        A * A + B * B =:= C * C].

perms([]) -> [[]];
perms(L) -> [[H | T] || H <- L, T <- perms(L -- [H])].

odds_and_even(L) -> odds_and_evens_acc(L, [], []).

odds_and_evens_acc([H | T], Odds, Evens) ->
    case H rem 2 of
        1 -> odds_and_evens_acc(T, [H | Odds], Evens);
        0 -> odds_and_evens_acc(T, Odds, [H | Evens])
    end;
odds_and_evens_acc([], Odds, Evens) ->
    {lists:reverse(Odds), lists:reverse(Evens)}.

sleep(T) -> receive  after T -> true end.

flush_buffer() ->
    receive _Any -> flush_buffer() after 0 -> true end.

priority_receive() ->
    receive
        {alarm, X} -> {alarm, X}
        after 0 -> receive Any -> Any end
    end.

on_exit(Pid, Fun) ->
    spawn(fun () ->
                  Ref = monitor(process, Pid),
                  receive {'DOWN', Ref, process, Pid, Why} -> Fun(Why) end
          end).

keep_alive(Name, Fun) ->
    register(Name, Pid = spawn(Fun)),
    on_exit(Pid, fun (_Why) -> keep_alive(Name, Fun) end).

consult(File) ->
    case file:open(File, read) of
        {ok, S} ->
            Val = consult1(S),
            file:close(S),
            {ok, Val};
        {error, Why} -> {error, Why}
    end.

consult1(S) ->
    case io:read(S, '') of
        {ok, Term} -> [Term | consult1(S)];
        eof -> [];
        Error -> Error
    end.

unconsult(File, L) ->
    {ok, S} = file:open(File, write),
    lists:foreach(fun (X) -> io:format(S, "~p.~n", [X]) end,
                  L),
    file:close(S).

-include_lib("kernel/include/file.hrl").

file_size_and_type(File) ->
    case file:read_file_info(File) of
        {ok, Facts} ->
            {Facts#file_info.type, Facts#file_info.size};
        _ -> error
    end.

ls(Dir) ->
    {ok, L} = file:list_dir(Dir),
    lists:map(fun (I) -> {I, file_size_and_type(I)} end,
              lists:sort(L)).

string2value(Str) ->
    {ok, Tokens, _} = erl_scan:string(Str ++ "."),
    {ok, Exprs} = erl_parse:parse_exprs(Tokens),
    Bindings = erl_eval:new_bindings(),
    {value, Value, _} = erl_eval:exprs(Exprs, Bindings),
    Value.
