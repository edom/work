:- module(plant, [

    plant_data/1
    , plant_property/3

    % Convenience

    , plant_scientific_name/2
    , plant_day_to_harvest/2
    , plant_harvest_idr_per_kg/2
    , plant_harvest_kg_per_plant/2
    , plant_dimension/4

    % Computed properties

    , plant_land_area/2
    , plant_revenue_per_land_area/2

]).

:- use_module(library(clpq)).

/*
This data may be incorrect.

- day_to_harvest(Int): the number of days between the planting of the seed and the harvesting of the plant.
- harvest_idr_per_kg(Int): average price, in IDR, of each kg of harvested plant part (only the part that can be sold).
- harvest_kg_per_plant(Float): average amount of harvestable part per adult plant individual.
- dimension([Length, Width, Height]): per adult individual, each axis in meter.
  - Length should be greater than or equal to Width.
*/

plant_data([
    [potato
        , scientific_name('Solanum tuberosum')
        , day_to_harvest(270)
        , harvest_idr_per_kg(20000)
        , harvest_kg_per_plant(1.0)
        , dimension([0.3, 0.3, 0.5])
    ]
    ,
    [tomato
        , scientific_name('Solanum lycopersicum')
        , day_to_harvest(270)
        , harvest_idr_per_kg(10000)
        , harvest_kg_per_plant(1.0)
        , dimension([0.3, 0.3, 1.0])
    ]
    ,
    [cabai
        , scientific_name('Capsicum annuum')
        , day_to_harvest(270)
        , harvest_idr_per_kg(40000)
        , harvest_kg_per_plant(1.0)
        , dimension([0.4, 0.4, 0.5])
    ]
    ,
    [cabai_rawit
        , scientific_name('Capsicum frutescens')
        , day_to_harvest(270)
        , harvest_idr_per_kg(30000)
        , harvest_kg_per_plant(1.0)
    ]
    ,
    [garlic
        , scientific_name('Allium sativum')
    ]
    ,
    [leek
        , scientific_name('Allium ampeloprasum')
    ]
]).

plant_property(Name, Property, Value) :- true
    , plant_data(Data)
    , member([Name | Plant], Data)
    , member(Term, Plant)
    , Term =.. [Property, Value]
    .

plant_scientific_name(Name, Scientific_name) :- plant_property(Name, scientific_name, Scientific_name).
plant_day_to_harvest(Name, Day) :- plant_property(Name, day_to_harvest, Day).
plant_harvest_idr_per_kg(Name, IDR) :- plant_property(Name, harvest_idr_per_kg, IDR).
plant_harvest_kg_per_plant(Name, Amount) :- plant_property(Name, harvest_kg_per_plant, Amount).
plant_dimension(Name, Length, Width, Height) :- plant_property(Name, dimension, [Length, Width, Height]).

plant_land_area(Name, Area) :- plant_dimension(Name, Length, Width, _), {Area = Length * Width}.

plant_revenue_per_land_area(Name, Amount) :- true
    , plant_harvest_idr_per_kg(Name, Idr_per_kg)
    , plant_harvest_kg_per_plant(Name, Kg_per_plant)
    , plant_land_area(Name, Area)
    , {Amount = Idr_per_kg * Kg_per_plant / Area}
    .

example :-
    member(Case, [
        % My house, starting out.
        (Shitter =< 5, Farm_area =< 10),
        % This is if I manage to buy my neighbor's house, and convince my neighbors to give their shit to me.
        (Shitter =< 15, Farm_area =< 100),
        % This is if the whole Jakarta (10 million people, 661.5 km2) becomes a farm.
        (Shitter =< 10 000 000, Farm_area =< 1000 * 1000 * 661)
    ]),
    {
        % Variables we want the computer to compute.

        Shitter >= 0, % person
        Farm_area >= 0, % m2
        Plant = min(Max_farm_plant_by_shit, Max_farm_plant_by_area),
        Laborer_frac = Labor / Plant_mature_day / Laborer_hour_per_day,

        Case, % We calculate each of the cases described above.

        % Constants depending on people and plants.
        Person_shit = 300, % gram/person-day
        Plant_area = 20 * 20, % cm2/plant
        Plant_mature_mass = 300, % gram/plant
        Plant_mature_day = 60, % day
        Plant_growth_rate = Plant_mature_mass / Plant_mature_day, % gram/plant-day
        Plant_usd_per_kg = 2, % USD/kg
        Plant_labor = 300, % labor required to sow+tend+harvest+clean+weigh+wrap+label+transport, in man-second/plant

        Laborer_wage = 700, % USD/month
        Laborer_hour_per_day = 15, % averaged man-hour/calendar day (not working day)

        % Derived constraints.
        Max_farm_growth_rate = Shitter * Person_shit, % gram/day
        Max_farm_plant_by_shit = Max_farm_growth_rate / Plant_growth_rate,
        Max_farm_plant_by_area = 100 * 100 * Farm_area / Plant_area,
        Labor = Plant * Plant_labor / 3600, % man-hour

        % Shitter is oversimplified model of biogeochemical nutrient cycle.
        % We should model major nutrients separately.

        % Fertilizer costs?
        % Other operations? Selecting/weighing/wrapping/labeling/transporting? Transporting to market?

        0 = 0
    },
    maximize(Max_farm_plant_by_shit),
    maximize(Max_farm_plant_by_area),
    maximize(Plant),
    Laborer is max(0, ceil(Laborer_frac) - 1), % That one person is the company owner who doesn't receive wages.
    % Accounting
    Gross_income is Plant * Plant_usd_per_kg / (Plant_mature_day / 30), % USD/month
    Wages is Laborer * Laborer_wage,
    Consignment_fees is Gross_income * 50 / 100, % consignment
    Expenses is Wages + Consignment_fees,
    Net_income is Gross_income - Expenses,
    write({
        shitter:Shitter,
        area:Farm_area,
        plant:Plant,
        laborer:Laborer,
        accounting_monthly:{
            gross_income:Gross_income,
            expenses:{
                wages:Wages,
                consignment_fees:Consignment_fees,
                total:Expenses
            },
            net_income:Net_income
        },
        labor:Labor,
        plant_max:{by_shit:Max_farm_plant_by_shit, by_area:Max_farm_plant_by_area}
    }).
