% -------------------- encyclopedia

%%  encyclopedia(Thing, Inputs, Producer, Producability)
%   Producability is units per hour.

% agriculture and food
encyclopedia(seed, 0.1*water, plantation, 889.63).
encyclopedia(apple, 3*water + 1*seed, plantation, 202.19).
encyclopedia(orange, 3*water + 1*seed, plantation, 186.01).
encyclopedia(grape, 4*water + 1*seed, plantation, 161.75).
encyclopedia(grain, 0.5*water + 1*seed, plantation, 808.75).
encyclopedia(steak, 8*water + 10*grain, farm, 45.21).
encyclopedia(sausage, 3*water + 2*grain, farm, 135.63).
encyclopedia(egg, 0.4*water + 0.5*grain, farm, 316.47).
encyclopedia(cotton, 1*water + 1*seed, plantation, 258.80).

% fashion
encyclopedia(fabric, 2*cotton + 1*power, clothes_factory, 241.12).
encyclopedia(leather, 5*water + 10*grain, farm, 30.14).
encyclopedia(underwear, 1*fabric, clothes_factory, 167.45).
encyclopedia(glove, 0.5*fabric + 0.5*leather, clothes_factory, 144.61).
encyclopedia(dress, 3*fabric + 0.5*plastic, clothes_factory, 152.22).
encyclopedia(shoe, 1*leather + 0.2*plastic, clothes_factory, 98.95).
encyclopedia(handbag, 1.5*leather, clothes_factory, 68.50).
encyclopedia(sneaker, 1*plastic, clothes_factory, 175.06).

% energy
encyclopedia(petrol, 15*power + 1*crude_oil, refinery, 112.54).

% electronics
encyclopedia(smartphone, 2*processor + 1*electronic_component + 1*battery + 1*display + 2*aluminium, electronics_factory, 11.6).

% automotive
encyclopedia(economy_car, 1*combustion_engine + 1*car_interior + 1*car_body + 1*on_board_computer, car_factory, 14.09).

encyclopedia(water, 0.2*power, reservoir, 1626.43).
encyclopedia(power, nothing, power_plant, 2592.87).

thing(A) :- encyclopedia(A, _, _, _).
thing_inputs(Output, Inputs) :- encyclopedia(Output, Inputs, _, _).
produces(Producer, Thing) :- encyclopedia(Thing, _, Producer, _).
producability(Thing, Rate) :- encyclopedia(Thing, _, _, Rate).

% -------------------- hourly labor cost

labor(plantation, 105).
labor(farm, 138).
labor(grocery, 143).
labor(reservoir, 345).
labor(power_plant, 414).
labor(clothes_factory, 143).
labor(fashion_store, 321).
labor(electronics_factory, 380).
labor(electronics_store, 173).
labor(refinery, 483).
labor(car_dealership, 380).
labor(gas_station, 345).

% -------------------- exchange

% no data for sellability

%%  market(Thing, Sellability, Price_per_unit)

market(nothing, 0, 0).

% agriculture and food

market(seed, 10000, 0.220).
market(apple, 1000, 2.000).
market(orange, 1000, 2.100).
market(grape, 1000, 2.500).
market(grain, 1000, 0.640).
market(steak, 100, 12.500).
market(sausage, 500, 3.850).
market(egg, 1000, 1.100).
market(cotton, 1000, 1.300).

% fashion

market(fabric, 1000, 3.900).
market(leather, 500, 14.500).
market(underwear, 1000, 4.900).
market(glove, 100, 11.400).
market(dress, 500, 19.400).
market(shoe, 1000, 20.000).
market(handbag, 500, 25.250).
market(sneaker, 1000, 13.700).

market(petrol, 1000, 39.500).

market(smartphone, 1000, 620). % Q3

market(economy_car, 1000, 2100.000).

market(water, 10000, 0.305).
market(power, 10000, 0.239).

market_sellability(Thing, Rate) :- market(Thing, Rate, _).
market_price(Thing, Price) :- market(Thing, _, Price).

% -------------------- retail

%%  sells(Place, Thing, Sellability, Price).

sells(grocery, apple, 89.04, 4.25).
sells(grocery, orange, 78.95, 4.66).
sells(grocery, grape, 69.34, 5.39).
sells(grocery, steak, 24.28, 20.58).
sells(grocery, sausage, 93.26, 5.85).
sells(grocery, egg, 427.05, 1.53).
sells(fashion_store, underwear, 24.21, 22.13).
sells(fashion_store, glove, 18.62, 33.68).
sells(fashion_store, dress, 42.85, 27.62).
sells(fashion_store, shoe, 27.21, 34.57).
sells(fashion_store, handbag, 15.20, 51.37).
sells(fashion_store, sneaker, 30.86, 26.80).
sells(gas_station, petrol, 79.63, 45.06).
sells(electronics_store, smartphone, 3.07, 706.25).
sells(car_dealership, economy_car, 1.54, 2192.39).

sells(Place, Thing) :- sells(Place, Thing, _Sellability, _Price).

retail_price(Thing, Price) :- sells(_Place, Thing, _Sellability, Price).

sellability(Thing, Rate) :- sells(_, Thing, Rate, _).

% -------------------- search

% Constraints that limit the search.
:- dynamic(must_buy/1).
must_buy(seed).
must_buy(cotton).
must_buy(power).
must_buy(water).
must_buy(grain).
must_buy(leather).
must_buy(fabric).

:- dynamic(limit_things_to/1).
% retail items
%limit_things_to([apple,orange,grape]).
% raw materials
%limit_things_to([seed,cotton,grain,fabric,leather,water,power]).

procure(A+B, Z) :- !, Z = X+Y, procure(A,X), procure(B,Y).
procure(A*B, Z) :- !, Z = A*X, procure(B,X).
procure(Thing, buy(Thing)) :- market_price(Thing, _).
procure(Thing, produce(Thing, Subprocurements)) :-
    \+ must_buy(Thing),
    thing_inputs(Thing, Inputs),
    procure(Inputs, Subprocurements).

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
