% -------------------- encyclopedia

%%  encyclopedia(Thing, Inputs, Producer, Producability)
%   Producability is units per hour.

encyclopedia(seed, 0.1*water, plantation, 889.63).
encyclopedia(apple, 3*water + 1*seed, plantation, 202.19).
encyclopedia(orange, 3*water + 1*seed, plantation, 186.01).
encyclopedia(grape, 4*water + 1*seed, plantation, 161.75).
encyclopedia(grain, 0.5*water + 1*seed, plantation, 808.75).
encyclopedia(cotton, 1*water + 1*seed, plantation, 258.80).
encyclopedia(fabric, 2*cotton + 1*power, clothes_factory, 241.12).
encyclopedia(leather, 5*water + 10*grain, farm, 30.14).
encyclopedia(underwear, 1*fabric, clothes_factory, 167.45).
encyclopedia(water, 0.2*power, reservoir, 1626.43).
encyclopedia(steak, 8*water + 10*grain, farm, 45.21).
encyclopedia(sausage, 3*water + 2*grain, farm, 135.63).
encyclopedia(egg, 0.4*water + 0.5*grain, farm, 316.47).
encyclopedia(power, nothing, power_plant, 2592.87).

thing(A) :- encyclopedia(A, _, _, _).
thing_inputs(Output, Inputs) :- encyclopedia(Output, Inputs, _, _).
produces(Producer, Thing) :- encyclopedia(Thing, _, Producer, _).
producability(Thing, Rate) :- encyclopedia(Thing, _, _, Rate).

% -------------------- hourly labor cost

labor(plantation, 105).
labor(farm, 138).
labor(grocery, 140).
labor(reservoir, 345).
labor(power_plant, 414).
labor(clothes_factory, 138).
labor(fashion_store, 311).

% -------------------- exchange

% no data for sellability

%%  market(Thing, Sellability, Price_per_unit)

market(nothing, 0, 0).
market(seed, 10000, 0.220).
market(apple, 1000, 2.200).
market(orange, 1000, 2.100).
market(grape, 1000, 2.500).
market(grain, 1000, 0.640).
market(steak, 100, 12.500).
market(sausage, 500, 3.850).
market(egg, 1000, 1.100).
market(cotton, 1000, 1.300).
market(water, 10000, 0.309).
market(fabric, 1000, 5.000).
market(leather, 500, 14.400).
market(underwear, 1000, 4.950).
market(power, 10000, 0.300).

market_sellability(Thing, Rate) :- market(Thing, Rate, _).
market_price(Thing, Price) :- market(Thing, _, Price).

% -------------------- retail

sells(grocery, apple, 89.04, 4.25).
sells(grocery, orange, 78.95, 4.66).
sells(grocery, grape, 69.34, 5.39).
sells(grocery, steak, 24.28, 20.58).
sells(grocery, sausage, 93.26, 5.85).
sells(grocery, egg, 427.05, 1.53).
sells(fashion_store, underwear, 24.39, 22.03).

sells(Place, Thing) :- sells(Place, Thing, _Sellability, _Price).

retail_price(Thing, Price) :- sells(_Place, Thing, _Sellability, Price).

sellability(Thing, Rate) :- sells(_, Thing, Rate, _).

% -------------------- search

% Constraints that limit the search.
:- dynamic(must_buy/1).
must_buy(power).
must_buy(water).

procure(A+B, Z) :- !, Z = X+Y, procure(A,X), procure(B,Y).
procure(A*B, Z) :- !, Z = A*X, procure(B,X).
procure(Thing, buy(Thing)) :- market_price(Thing, _).
procure(Thing, produce(Thing, Subprocurements)) :-
    \+ must_buy(Thing),
    thing_inputs(Thing, Inputs),
    procure(Inputs, Subprocurements).

:- dynamic(limit_things_to/1).
% limit_things_to([cotton,seed,fabric]).

interesting_thing(Thing) :-
    (limit_things_to(Things)
    ->  member(Thing,Things)
    ;   thing(Thing)
    ).

sell(Profit_per_hour, retail(Procurement)) :-
    interesting_thing(Thing),
    procure(Thing, Procurement),
    cost(Procurement, Procurement_cost),
    sells(Seller, Thing),
    labor(Seller, Labor_cost_per_hour),
    sellability(Thing, Sellability),
    producability(Thing, Producability),
    Sale_per_hour is min(Producability,Sellability),
    Selling_cost is Labor_cost_per_hour / Sale_per_hour,
    Cost is Selling_cost + Procurement_cost,
    retail_price(Thing, Retail_price),
    Profit_per_unit is Retail_price - Cost,
    Profit_per_hour is Sale_per_hour * Profit_per_unit.

sell(Profit_per_hour, market(Procurement)) :-
    interesting_thing(Thing),
    procure(Thing, Procurement),
    cost(Procurement, Procurement_cost),
    market_sellability(Thing, Market_sellability),
    producability(Thing, Producability),
    Sale_per_hour is min(Producability,Market_sellability),
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

