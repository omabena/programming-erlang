-module(primer_server).

-behaviour(gen_server).

-export([new_prime/1, start_link/0]).

%% gen_server callbacks
-export([code_change/3,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         init/1,
         terminate/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE},
                          ?MODULE,
                          [],
                          []).

new_prime(N) ->
    %% 20000 is a timeout (ms)
    gen_server:call(?MODULE, {prime, N}, 20000).

init([]) ->
    process_flag(trap_exit, true),
    io:format("~p starting~n", [?MODULE]),
    {ok, 0}.

handle_call({prime, K}, _From, N) ->
    {reply, make_new_prime(K), N + 1}.

handle_cast(_Msg, N) -> {noreply, N}.

handle_info(_Info, N) -> {noreply, N}.

terminate(_Reason, _N) ->
    io:format("~p stopping~n", [?MODULE]),
    ok.

code_change(_OldVsn, N, _Extra) -> {ok, N}.

make_new_prime(K) ->
    if K > 100 ->
           alarm_handler:set_alarm(tooHot),
           N = lib_primes:make_prime(K),
           alarm_handler:clear_alarm(tooHot),
           N;
       true -> lib_primes:make_prime(K)
    end.
