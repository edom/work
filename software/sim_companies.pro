%   We assume that the user has downloaded the data using sim_companies.js.

:-  consult('./sim_companies_static.pro').
:-  consult('./sim_companies_dynamic.pro').

% -------------------- constraints

:- dynamic(have_building/1).

have_building(B) :- building_name(B, 'Water reservoir').
have_building(B) :- building_name(B, 'Power plant').
%have_building(B) :- building_name(B, 'Mine').
%have_building(B) :- building_name(B, 'Oil rig').
have_building(B) :- building_name(B, 'Refinery').
have_building(B) :- building_name(B, 'Shipping depot').
have_building(B) :- building_name(B, 'Plantation').
have_building(B) :- building_name(B, 'Farm').
%have_building(B) :- building_name(B, 'Beverage factory').
have_building(B) :- building_name(B, 'Factory').
have_building(B) :- building_name(B, 'Fashion factory').
%have_building(B) :- building_name(B, 'Car factory').
%have_building(B) :- building_name(B, 'Electronics factory').
have_building(B) :- building_name(B, N), member(N, [
    'Plant research center'
    , 'Physics laboratory'
    , 'Breeding laboratory'
    , 'Chemistry laboratory'
    , 'Software R&D'
    , 'Automotive R&D'
    , 'Fashion & Design'
]).

%can_produce(R) :- resource_name(R, 'Minerals').
%can_produce(R) :- resource_name(R, 'Iron ore').
%can_produce(R) :- resource_name(R, 'Sand').
can_produce(R) :- have_building(B), building_produces_resource(B, R).

:- dynamic(cannot_produce/1).

cannot_produce(R) :- resource_name(R, 'Seeds').
cannot_produce(R) :- resource_name(R, 'Eggs').
cannot_produce(R) :- resource_name(R, 'Grain').
%cannot_produce(R) :- resource_name(R, 'Diesel').
cannot_produce(R) :- resource_name(R, 'Power').
%cannot_produce(R) :- resource_name(R, 'Bauxite').
%cannot_produce(R) :- resource_name(R, 'Sand').
cannot_produce(R) :- resource_name(R, 'Water').
%cannot_produce(R) :- resource_name(R, 'Chemicals').
%cannot_produce(R) :- resource_name(R, 'Silicon').
%cannot_produce(R) :- resource_name(R, 'Minerals').
%cannot_produce(R) :- resource_name(R, 'Iron ore').
cannot_produce(R) :- resource_name(R, 'Fabric').
cannot_produce(R) :- resource_name(R, 'Leather').
%cannot_produce(R) :- resource_name(R, 'Plastic').
%cannot_produce(R) :- resource_name(R, 'Golden bars').
cannot_produce(R) :- resource_name(R, 'Necklace').
cannot_produce(R) :- resource_name(R, 'Luxury watch').

administration_overhead(0.0882).

%   Must not be zero.
%   When in doubt, put the lowest abundance of all used mines.
mine_abundance(0.8).

% -------------------- accessors

resource(A) :- resource(A, _, _).
resource_name(A, B) :- resource(A, B, _).

building_name(A, B) :- building(A, B, _, _, _, _).
building_wage(A, B) :- building(A, _, B, _, _, _).

building_produces_resource(A, B) :- building_produces_resource(A, B, _).

producing_requires(Out, In) :- producing_requires(Out, _, In).

% -------------------- action generation

procure([], []).

procure([Count*Out|Outs], [A|B]) :-
    procure1(Count, Out, A),
    procure(Outs, B).

procure1(Count, Out, buy(Count, Out, Market_Price, Cost)) :-
    resource_market_price(Out, Market_Price),
    Cost is Count * Market_Price.

procure1(Count, Out, produce(Count, Out, Building, Total_Cost, Labor_Cost, Material_Cost, Actions)) :-
    resource(Out),
    can_produce(Out),
    \+ cannot_produce(Out),
    findall(N*In, (producing_requires(Out, InPerOut, In), N is Count*InPerOut), Ins),
    building_produces_resource(Building, Out, Full_Out_Per_Hour),
    (Building == 'M'    % mine
    ->  mine_abundance(Abundance)
    ;   Abundance = 1
    ),
    Out_Per_Hour is Abundance * Full_Out_Per_Hour,
    building_wage(Building, Wage),
    administration_overhead(Adm_Overhead),
    Labor_Cost is (1 + Adm_Overhead) * Count * Wage / Out_Per_Hour,
    procure(Ins, Actions),
    actions_total_cost(Actions, Material_Cost),
    Total_Cost is Material_Cost + Labor_Cost.

actions_total_cost([], 0).
actions_total_cost([A|B], CAB) :-
    action_total_cost(A, CA),
    actions_total_cost(B, CB),
    CAB is CA + CB.

action_total_cost(buy(_, _, _, Cost), Cost).
action_total_cost(produce(_, _, _, Cost, _, _, _), Cost).

action_profit_per_hour(retail(_, _, _, Profit, _, _, _, _), Profit).
action_profit_per_hour(market(_, _, Profit, _, _, _, _, _, _), Profit).

action_profit_per_cost(retail(_, _, Return, _, _, _, _, _), Return).
action_profit_per_cost(market(_, Return, _, _, _, _, _, _, _), Return).

action_resource(market(Resource, _, _, _, _, _, _, _, _), Resource).
action_resource(retail(Resource, _, _, _, _, _, _, _), Resource).

resource_transport(A, B) :- resource(A, _, B).

% all profits, revenues, and costs are per unit, unless mentioned otherwise
sell(retail(Out, Building, Profit_Per_Cost, Profit_Per_Hour, Profit, Revenue, Cost, Proc)) :-
    procure1(1, Out, Proc),
    building_sells_resource(Building, Out, Sale_Per_Hour, Avg_Price),
    building_wage(Building, Wage),
    Revenue is Avg_Price,
    administration_overhead(Adm_Overhead),
    Labor_Cost is (1 + Adm_Overhead) * Wage / Sale_Per_Hour,
    action_total_cost(Proc, Material_Cost),
    Cost is Labor_Cost + Material_Cost,
    Profit is Revenue - Cost,
    Profit_Per_Cost is Profit / Cost,
    Profit_Per_Hour is Sale_Per_Hour * Profit.

sell(market(Out, Profit_Per_Cost, Profit_Per_Hour, Profit, Revenue, Cost, Transport_Cost, Market_Fee, Proc)) :-
    procure1(1, Out, Proc),
    Proc \= buy(_, _, _, _),
    action_total_cost(Proc, Material_Cost),
    resource_transport(Out, Transport),
    resource_market_price(13, Unit_Transport_Cost), % resource 13 is Transport
    Transport_Cost is Transport * Unit_Transport_Cost,
    Market_Fee is 0.03 * Material_Cost,
    Cost is Material_Cost + Transport_Cost + Market_Fee,
    resource_market_price(Out, Revenue),
    Profit is Revenue - Cost,
    Profit_Per_Cost is Profit / Cost,
    market_sale_per_hour(Out, Sale_Per_Hour),
    Profit_Per_Hour is Sale_Per_Hour * Profit.

% guess the minimum between production capacity and market demand
/*
market_sale_per_hour(R, 1000) :- resource_name(R, 'Power'), !.
market_sale_per_hour(R, 123) :- resource_name(R, 'Glass'), !.
market_sale_per_hour(R, 200) :- resource_name(R, 'Chemicals'), !.
market_sale_per_hour(R, 150) :- resource_name(R, 'Silicon'), !.
market_sale_per_hour(R, 180) :- resource_name(R, 'Steel'), !.
market_sale_per_hour(R, 100) :- resource_name(R, 'Minerals'), !.
*/
market_sale_per_hour(R, Q) :- building_produces_resource(_, R, Q).

show_actions(_, []) :- !.
show_actions(D, [A|B]) :- !, show_action(D,A), show_actions(D,B).

show_action(Indent, retail(R, B, Profit_Per_Cost, Profit_Per_Hour, Profit, Revenue, Cost, Proc)) :- !,
    resource_name(R, Name),
    building_name(B, Building),
    format('~0|~*|retail ~w at ~w, P/C ~3f% @ $~3f/hr, profit $~3f, revenue $~3f, cost $~3f~n',
        [Indent, Name, Building, 100*Profit_Per_Cost, Profit_Per_Hour, Profit, Revenue, Cost]),
    Indent1 is Indent + 2,
    show_action(Indent1, Proc).

show_action(Indent, market(R, Profit_Per_Cost, Profit_Per_Hour, Profit, Revenue, Cost, Transport_Cost, Market_Fee, Proc)) :- !,
    resource_name(R, Name),
    format('~0|~*|market ~w, P/C ~3f% @ $~3f/hr, profit $~3f, revenue $~3f, cost $~3f, transport $~3f, fee $~3f~n',
        [Indent, Name, 100*Profit_Per_Cost, Profit_Per_Hour, Profit, Revenue, Cost, Transport_Cost, Market_Fee]),
    Indent1 is Indent + 2,
    show_action(Indent1, Proc).

show_action(Indent, buy(Count, R, Market_Price, Material_Cost)) :- !,
    resource_name(R, Name),
    format('~0|~*|buy ~w ~w at $~3f, material $~3f~n',
        [Indent, Count, Name, Market_Price, Material_Cost]).

show_action(Indent, produce(Count, R, B, Total_Cost, Labor_Cost, Material_Cost, Actions)) :- !,
    resource_name(R, Name),
    building_name(B, Building),
    format('~0|~*|produce ~w ~w in ~w, total $~3f, labor $~3f, material $~3f~n',
        [Indent, Count, Name, Building, Total_Cost, Labor_Cost, Material_Cost]),
    Indent1 is Indent + 2,
    show_actions(Indent1, Actions).

show_action(Indent, A) :- !,
    format('~0|~*|unknown step: ~w~n', [Indent, A]).

%%  sort_helper(Resource, Dest, Key, Action)
%
%   Dest is market or retail.
:- dynamic(sort_helper/4).

main :-
    %   If deduplication by resource is wanted, use this:
    retractall(sort_helper/4),
    forall(sell(Action), (
        functor(Action, Dest, _), % market or retail
        action_resource(Action, Resource),
        %   If you have little cash, pick the action with the highest profit-per-COST.
        % action_profit_per_cost(Action, Key),
        %   If you have excess cash, pick the action with the highest profit-per-TIME.
        action_profit_per_hour(Action, Key),
        (sort_helper(Resource, Dest, Max_Key, _)
        ->  (Key > Max_Key
            ->  retractall(sort_helper(Resource, Dest, _, _)),
                assertz(sort_helper(Resource, Dest, Key, Action))
            ;   true
            )
        ;   assertz(sort_helper(Resource, Dest, Key, Action))
        )
    )),
    findall(key(Key,Dest)-Action, sort_helper(_, Dest, Key, Action), Pairs),
    %   Otherwise, if deduplication by resource is not wanted, use this:
    %findall(Key-Action, (sell(Action), action_profit_per_cost(Action, Key)), Pairs),
    %   End if.
    sort(Pairs, Ordered),
    forall(member(_-Action, Ordered), (
        show_action(0, Action),
        nl
    )).
