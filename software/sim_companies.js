//  Usage:
//
//  -   Use the Chromium browser to log into your Sim Companies account.
//
//  -   Uncomment exactly one of "get_static_data" or "get_dynamic_data" at the end of this script.
//
//  -   Copy the script into the Chromium Console.
//
//  -   Wait for the browser to do its job.
//
//  -   Copy the data from the clipboard to a Prolog source file.

//  Chromium-specific: Copy the data to the clipboard
//  "copy" is only accessible from the top-level.
COPY__ = copy

function COPY (data) {
    COPY__(data)
    console.log("Data copied to clipboard.")
}

// Assume that atoms do not contain apostrophes.
function prolog_atom (s) {
    return "'" + s + "'"
}

//  fetch_json : Url -> (Json -> a) -> Promise (Promise a)
function fetch_json (url, handler) {
    return fetch(url, {
        "credentials": "include",
        "headers": {"accept":"application/json, text/plain, */*"},
        "referrer": "https://www.simcompanies.com/",
        "referrerPolicy": "origin",
    }).then(response => {
        if(response.status != 200) {
            throw `error: status != 200: ${url}`
        }
        return response.json().then(handler)
    })
};

//  Buildings and resources.

function get_static_data () {
    //  Prolog facts.
    var BUILDING = [] // building exists
    var RESOURCE = [] // resource exists
    var PRODUCING_REQUIRES = []
    var BUILDING_PRODUCES_RESOURCE = []
    var BUILDING_SELLS_RESOURCE = []

    function write_prolog () {
        var prolog = ''
        prolog += '%%  building(Id, Name, Wage, Cost, Steel, Second_To_Build)\n\n'
        BUILDING.forEach(building => {
            let { id, name, wage, cost, steel, secondsToBuild } = building
            prolog += `building(${prolog_atom(id)}, ${prolog_atom(name)}, ${wage}, ${cost}, ${steel}, ${secondsToBuild}).\n`
        })
        prolog += "\n"
        prolog += `%%  resource(Id, Name, Transport)\n\n`
        RESOURCE.forEach(resource => {
            let { id, name, transportNeeded, } = resource
            prolog += `resource(${id}, ${prolog_atom(name)}, ${transportNeeded}).\n`
        })
        prolog += "\n"
        prolog += `\
%%  producing_requires(Out, Count, In)
%
%   Producing an instance of Out requires Count instances of In.

`
        PRODUCING_REQUIRES.forEach(producing_requires => {
            let { outputId, inputAmount, inputId } = producing_requires
            prolog += `producing_requires(${outputId}, ${inputAmount}, ${inputId}).\n`
        })
        prolog += "\n"
        prolog += `%%  building_produces_resource(Building_Id, Res_Id, Per_Hour)\n\n`
        BUILDING_PRODUCES_RESOURCE.forEach(row => {
            let { buildingId, resourceId, anHour } = row
            prolog += `building_produces_resource(${prolog_atom(buildingId)}, ${resourceId}, ${anHour}).\n`
        })
        prolog += "\n"
        prolog += `%%  building_sells_resource(Store_Id, Res_Id, Sale_Per_Hour, Avg_Price)\n\n`
        BUILDING_SELLS_RESOURCE.forEach(sale => {
            let { buildingId, resourceId, salePerHour, averageRetailPrice } = sale
            prolog += `building_sells_resource(${prolog_atom(buildingId)}, ${resourceId}, ${salePerHour}, ${averageRetailPrice}).\n`
        })
        prolog += "\n"
        return prolog
    }

    //  Opaque functions copied from Chromium-prettified website source code.
    let SimCompanies = (function () {
        let timeModeling = function (retailModeling, saturation, amount, price) {
            return eval(retailModeling)
        }
        let timeToSellUnits = function (amount, salesModifier, price, quality, marketSaturation, retailModeling) {
            let saturation = Math.max(marketSaturation - .24 * quality, .1)
            let time = timeModeling(retailModeling, saturation, amount, price)
            return Math.ceil(time - time * salesModifier / 100)
        }
        let unitsSoldAnHour = function (salesModifier, price, quality, marketSaturation, retailModeling) {
            // salesModifier is in percentage; 2 means 2%
            var timePerUnit = timeToSellUnits(100, salesModifier, price, quality, marketSaturation, retailModeling)
            return 3600e2 / timePerUnit
        }
        //  This is what is displayed.
        let unitsAnHour = function (price, marketSaturation, retailModeling) {
            let salesModifier = 0
            let quality = 0
            let level = 1
            return level * unitsSoldAnHour(salesModifier, price, quality, marketSaturation, retailModeling)
        }
        return {
            unitsAnHour
        }
    })()

    function get_buildings () {
        return fetch_json("https://www.simcompanies.com/api/buildings/new", array => {
            array.forEach(building => {
                let { category, cost, kind, name, production, secondsToBuild, steel, wages } = building
                BUILDING.push({
                    id: kind,
                    name,
                    wage: wages, // per hour per level
                    cost,
                    steel,
                    secondsToBuild,
                })
                if (production) {
                    production.forEach(prod => {
                        let { anHour, resource } = prod
                        let { db_letter: resourceId } = resource
                        BUILDING_PRODUCES_RESOURCE.push({ buildingId: kind, resourceId, anHour, })
                    })
                }
            })
        })
    }

    function get_resource (resource_id) {
        return fetch_json(`https://www.simcompanies.com/api/v2/encyclopedia/resources/2/${resource_id}/`, resource => {
            //  Reverse-engineered from website JavaScript code.
            function timeModeling (retailModeling, saturation, amount, price) {
                return eval(retailModeling)
            }
            let { averageRetailPrice, baseSalary, db_letter: resourceId, improvesQualityOf,
                marketSaturation, marketSaturationLabel, name, neededFor, producedAnHour, producedAt, producedFrom,
                retailData, retailModeling, retailable, soldAt, transportNeeded, transportation } = resource
            let { db_letter: producerId } = producedAt
            RESOURCE.push({
                id: resourceId,
                name,
                transportNeeded,
            })
            producedFrom.forEach(factor => {
                PRODUCING_REQUIRES.push({
                    outputId: resourceId,
                    inputAmount: factor.amount,
                    inputId: factor.resource.db_letter
                })
            })
            if (retailable) {
                let salePerHour = SimCompanies.unitsAnHour(averageRetailPrice, marketSaturation, retailModeling)
                BUILDING_SELLS_RESOURCE.push({
                    buildingId: soldAt.db_letter,
                    resourceId,
                    salePerHour,
                    averageRetailPrice,
                })
            }
        })
    }

    function get_resource_ids () {
        return fetch_json("https://www.simcompanies.com/api/v2/encyclopedia/resources/",
            array => array.map(resource => resource.db_letter)
        )
    }

    let promises = [
        get_buildings()
        , get_resource_ids().then(resource_ids => {
            return Promise.all(resource_ids.map(get_resource))
        })
    ]

    Promise.all(promises).then(_ => {
        COPY(write_prolog())
    })
};

function get_dynamic_data () {
    //  Prolog facts.
    var RESOURCE_MARKET_PRICE = [] // snapshot of recent asking prices

    function write_prolog () {
        var prolog = ''
        prolog += `%%  resource_market_price(Res_Id, Price)\n\n`
        RESOURCE_MARKET_PRICE.forEach(row => {
            let { resourceId, price } = row
            prolog += `resource_market_price(${resourceId}, ${price}).\n`
        })
        prolog += "\n"
        return prolog
    }

    function get_market () {
        //  Get the current market price of everything that has been exchanged at least once between "since" and now.
        let since = new Date()
        since.setHours(since.getHours() - 7*24)
        return fetch_json(`https://www.simcompanies.com/api/v1/market-ticker/${since.toISOString()}/`, array => {
            array.forEach(item => {
                let { kind: resourceId, price } = item
                RESOURCE_MARKET_PRICE.push({ resourceId, price })
            })
        })
    }

    let promises = [
        get_market()
    ]

    Promise.all(promises).then(_ => {
        COPY(write_prolog())
    })

}

// get_static_data();
get_dynamic_data();

