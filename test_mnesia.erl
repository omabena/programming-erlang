-module(test_mnesia).

-import(lists, [foreach/2]).

-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-export([do_this_once/0]).

-record(shop, {item, quantity, cost}).

-record(cost, {name, price}).

-record(design, {id, plan}).

do_this_once() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(shop,
                        [{attributes, record_info(fields, shop)}]),
    mnesia:create_table(cost,
                        [{attributes, record_info(fields, cost)}]),
    mnesia:create_table(design,
                        [{attributes, record_info(fields, design)}]),
    mnesia:stop().

start() ->
    mnesia:start(),
    mnesia:wait_for_tables([shop, cost, design], 20000).

reset_tables() ->
    mnesia:clear_table(shop),
    mnesia:clear_table(cost),
    F = fun () ->
                foreach(fun mnesia:write/1, example_tables())
        end,
    mnesia:transaction(F).

demo(select_shop) ->
    do(qlc:q([X || X <- mnesia:table(shop)]));
demo(select_some) ->
    do(qlc:q([{X#shop.item, X#shop.quantity}
              || X <- mnesia:table(shop)]));
demo(reorder) ->
    do(qlc:q([X#shop.item
              || X <- mnesia:table(shop), X#shop.quantity < 250]));
demo(join) ->
    do(qlc:q([X#shop.item
              || X <- mnesia:table(shop), X#shop.quantity < 250,
                 Y <- mnesia:table(cost), X#shop.item =:= Y#cost.name,
                 Y#cost.price < 2])).

do(Q) ->
    F = fun () -> qlc:e(Q) end,
    {atomic, Val} = mnesia:transaction(F),
    Val.

example_tables() ->
    [%% The shop table
     {shop, apple, 20, 2.29999999999999982236},
     {shop, orange, 100, 3.79999999999999982236},
     {shop, pear, 200, 3.6},
     {shop, banana, 420, 4.5},
     {shop, potato, 2456, 1.19999999999999995559},
     %% The cost table
     {cost, apple, 1.5},
     {cost, orange, 2.39999999999999991118},
     {cost, pear, 2.2},
     {cost, banana, 1.5},
     {cost, potato, 5.99999999999999977796e-1}].

add_shop_item(Name, Quantity, Cost) ->
    Row = #shop{item = Name, quantity = Quantity,
                cost = Cost},
    F = fun () -> mnesia:write(Row) end,
    mnesia:transaction(F).

remove_shop_item(Item) ->
    Oid = {shop, Item},
    F = fun () -> mnesia:delete(Oid) end,
    mnesia:transaction(F).

farmer(Nwant) ->
    %% Nwant = Number of oranges the farmer wants to buy
    F = fun () ->
                %% find the number of apples
                [Apple] = mnesia:read({shop, apple}),
                Napples = Apple#shop.quantity,
                Apple1 = Apple#shop{quantity = Napples + 2 * Nwant},
                %% update the database
                mnesia:write(Apple1),
                %% find the number of oranges
                [Orange] = mnesia:read({shop, orange}),
                NOranges = Orange#shop.quantity,
                if NOranges >= Nwant ->
                       N1 = NOranges - Nwant,
                       Orange1 = Orange#shop{quantity = N1},
                       %% update the database
                       mnesia:write(Orange1);
                   true ->
                       %% Oops -- not enough oranges
                       mnesia:abort(oranges)
                end
        end,
    mnesia:transaction(F).

add_plans() ->
    D1 = #design{id = {joe, 1}, plan = {circle, 10}},
    D2 = #design{id = fred, plan = {rectangle, 10, 5}},
    D3 = #design{id = {jane, {house, 23}},
                 plan =
                     {house,
                      [{floor, 1, [{doors, 3}, {windows, 12}, {rooms, 5}]},
                       {floor, 2, [{doors, 2}, {rooms, 4}, {windows, 15}]}]}},
    F = fun () ->
                mnesia:write(D1),
                mnesia:write(D2),
                mnesia:write(D3)
        end,
    mnesia:transaction(F).

get_plan(PlanId) ->
    F = fun () -> mnesia:read({design, PlanId}) end,
    mnesia:transaction(F).
