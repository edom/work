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

:- use_module(library(clpr)).

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
