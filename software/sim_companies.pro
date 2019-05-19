thing(seed).
thing(water).
thing(apple).
thing(orange).
thing(grape).
thing(steak).
thing(sausage).
thing(egg).

% -------------------- hourly labor cost

labor(plantation, 105).
labor(grocery, 140).
labor(reservoir, 345).

% -------------------- production

produces(plantation, seed).
produces(plantation, apple).
produces(plantation, orange).
produces(plantation, grape).
produces(reservoir, water).

production(seed :- 0.1*water).
production(apple :- 3*water + 1*seed).
production(orange :- 3*water + 1*seed).
production(grape :- 4*water + 1*seed).
production(water :- nothing).

producability(seed, 889.63).
producability(apple, 202.19).
producability(orange, 186.01).
producability(grape, 161.75).
producability(water, 1626.43).

% -------------------- exchange

% market price per unit
market_price(nothing, 0).
market_price(seed, 0.189).
market_price(water, 0.309).
market_price(apple, 2.100).
market_price(orange, 2.000).
market_price(grape, 2.350).
market_price(steak, 13.000).
market_price(sausage, 3.700).
market_price(egg, 1.090).

% -------------------- retail

retail_price(apple, 4.25).
retail_price(orange, 4.66).
retail_price(grape, 5.39).
retail_price(steak, 20.58).
retail_price(sausage, 5.85).
retail_price(egg, 1.53).

sellability(apple, 89.04).
sellability(orange, 78.95).
sellability(grape, 69.34).
sellability(steak, 24.28).
sellability(sausage, 93.26).
sellability(egg, 427.05).

sells(grocery, apple).
sells(grocery, orange).
sells(grocery, grape).
sells(grocery, steak).
sells(grocery, sausage).
sells(grocery, egg).

% -------------------- no data

market_sellability(seed, 10000).
market_sellability(apple, 1000).
market_sellability(orange, 1000).
market_sellability(grape, 1000).
market_sellability(steak, 1000).
market_sellability(sausage, 1000).
market_sellability(egg, 1000).
market_sellability(water, 10000).

% -------------------- search

procure(A+B, Z) :- !, Z = X+Y, procure(A,X), procure(B,Y).
procure(A*B, Z) :- !, Z = A*X, procure(B,X).
procure(Thing, buy(Thing)) :- market_price(Thing, _).
procure(Thing, produce(Thing, Subprocurements)) :-
    production(Thing :- Inputs),
    procure(Inputs, Subprocurements).

sell(Profit_per_hour, retail(Procurement)) :-
    thing(Thing),
    procure(Thing, Procurement),
    cost(Procurement, Procurement_cost),
    sells(Seller, Thing),
    labor(Seller, Labor_cost_per_hour),
    sellability(Thing, Sale_per_hour),
    Selling_cost is Labor_cost_per_hour / Sale_per_hour,
    Cost is Selling_cost + Procurement_cost,
    retail_price(Thing, Retail_price),
    Profit_per_unit is Retail_price - Cost,
    Profit_per_hour is Sale_per_hour * Profit_per_unit.

sell(Profit_per_hour, market(Procurement)) :-
    thing(Thing),
    procure(Thing, Procurement),
    cost(Procurement, Procurement_cost),
    market_sellability(Thing, Sale_per_hour),
    market_price(Thing, Market_price),
    Selling_cost is 0.03 * Market_price,
    Cost is Selling_cost + Procurement_cost,
    Profit_per_unit is Market_price - Cost,
    Profit_per_hour is Sale_per_hour * Profit_per_unit.

main :-
    findall(Profit_per_hour-Procurement, sell(Profit_per_hour, Procurement), Processes),
    sort(Processes, Sorted_processes),
    forall(member(Profit_per_hour-Procurement, Sorted_processes),
        format('~0|~3f~16|~w~n', [Profit_per_hour, Procurement])
    ).

cost(N * Exp, Total) :- !,
    cost(Exp, Unit),
    Total is N * Unit.

cost(A + B, Total) :- !,
    cost(A, PA),
    cost(B, PB),
    Total is PA + PB.

cost(buy(Exp), P) :- !,
    market_price(Exp, P).

% unit
cost(produce(Thing, Inputs), Cost) :- !,
    produces(Producer, Thing),
    labor(Producer, Labor_cost_per_hour),
    cost(Inputs, Inputs_cost),
    producability(Thing, Production_per_hour),
    Labor_cost is Labor_cost_per_hour / Production_per_hour,
    Cost is Inputs_cost + Labor_cost.

