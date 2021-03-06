# rpwsdata
R package to access PWS data from Wunderground.
The rpwsdata package enables users to query the Wunderground project database from R and provide tools to visualize weather conditions for a place of choice.

* rpwsdata package provides functions to get available Personal Weather Stations(PWS) for any given place.

* rpwsdata package includes functions to access and download the current and historical weather of any place from each of the available PWS within the place.

* rpwsdata package returns current conditions data and historical data in data frames that can be used for further analysis

* rpwsdata package also includes interactive graphical interface to visualize the available PWS of a specified place and microclimate patterns of a place across the available PWS.

This document introduces you to rpwsdata functions and shows how to apply them to get weather data of a place:

## Create an object of WGround class

To access the weather data of a place from Wunderground project database, the package comes with a pre-defined 'WGround' class. All the functions provided by rpwsdata package work only on object of 'WGround' class.It is essential for the user to create an object of 'WGround' to use any of the package functionality.
An API key is required to query Wunderground, it can be obtained at https://www.wunderground.com/weather/api/d/pricing.html. Free Developer is available with restrictions on number of API calls.

`new()` allows you to create a new 'WGround' object:

For example, to create object to get PWS for San Francisco, CA.

```{r, eval=FALSE}
#library(rpwsdata)
obj <- new("WGround", APIkey=APIkey, city="San Francisco",state= "CA",country="USA")
obj <- new("WGround", APIkey=APIkey, city="San Francisco",state= "CA")
```

The object `obj` created above will be used for the remainder of this vignette to demonstrate other functionalities of rpwsdata

```{r, eval=FALSE}
obj <- new("WGround", APIkey=APIkey, city="San Francisco")
#Error in validObject(.Object) : 
#  invalid class "WGround" object: Location information provided not complete. State or Country needed for entered city.

obj <- new("WGround", APIkey=APIkey, city="San Francisco", country="US")
#Error in validObject(.Object) : 
#  invalid class "WGround" object: For US cities enter state information.
```

For cities inside US, 'country' is optional but 'state' must be provided. However for international cities, 'country' must be provided

```{r, eval=FALSE}
obj <- new("WGround", APIkey=APIkey, city="Paris",country="France")
```

## Obtain PWS for a given place

`getPWSinfo()` allows you to get all the available Personal Weather stations (PWS).

For example:

```{r, eval=FALSE}
obj <- getPWSinfo(obj)
```

This will obtain the PWS information from WunderGround web API and populates the `pws` slot of the Wground object.
`verbose` is optional argument to the function and it controls the printing of notes to the console. By default, verbose is on.
Setting `verbose = F` will turnoff any printing to console until occurence of a run time error.

```{r, eval = FALSE}
obj <- getPWSinfo(obj, verbose = F, distance_mi = 0.5, maxPWScount = 3)
```

The user can get only PWS within requested distance in miles of city (entered while creating the WGround object) using `distance_mi` argument.
This is optional and if not provided the getPWSinfo() will return all the availabe PWS for the requested city.
`maxPWScount` argument will limit PWS returned for the requested city.This is optional and returns all the available PWS if not provided in the function call.

## Obtain current weather from PWS stations.

`CurrentWeatherbyPWS()` enables users to access the latest weather information from each of the PWS sites contained in `pws` slot of the WGround object.
By default,The weather charateristics like 'weather', 'temp_f','relative_humidity', 'wind_mph', 'pressure_in', 'windchill_f', 'feelslike_f', 'visibility_mi' and 'precip_today_in' can be accessed for each of the PWS. This function returns'WGround' Object with `conditions` slot populated. 
```{r, eval = FALSE}
obj <- CurrentWeatherbyPWS(obj)
obj1 <- CurrentWeatherbyPWS(obj, verbose = F)
```

some possible wrong uses of the `CurrentWeatherbyPWS()` function.

```{r, eval = FALSE}
obj1 <- new("WGround", APIkey=APIkey, city="Denver", state="CO")
obj1 <- CurrentWeatherbyPWS(obj1)
#Error in CurrentWeatherbyPWS(obj) : ERROR.PWS info not provided 
```

The above error can be avoided by using the function in the right sequence as shown below.

```{r, eval = FALSE}
obj1 <- new("WGround", APIkey=APIkey, city="Denver", state="CO")
obj1 <- getPWSinfo(obj1)
obj1 <- CurrentWeatherbyPWS(obj1)
```

## Obtain Historical weather data of an entered city

`getHistoricalWeather()` fetches the historical weather between specified `startDt` and `endDt` (entered as yyyy/mm/dd format) for selected weather stations of the city contained in 'WGround' object provided as the main argument.The function returns a data frame with historical weather information.The argument `maxPWScount` limits the PWS sites and data is pulled only for required number of PWS sites.By default `maxPWScount` is set to 3 to limit the number of API calls to the wunder ground web API. Recently, Wunder ground imposed strict restrictions on the number of API calls and is important to use `maxPWScount=3` or less to prevent any future access issues. API calls more than 10 per minute results in blockage of the Developer API key.

```{r, eval = FALSE}
## startDt and endDt in yyyy/mm/dd format
weather_hist <- getHistoricalWeather(obj,startDt="2017/01/01",endDt="2017/01/02",maxPWScount=1)
```

some possible wrong uses of the `CurrentWeatherbyPWS()` function.

```{r, eval = FALSE}
obj1 <- new("WGround", APIkey=APIkey, city="Denver", state="CO")
obj1 <- getHistoricalWeather(obj1,startDt="2017/01/01",endDt="2017/01/02",maxPWScount=1,verbose=F)
#Error in getHistoricalWeather(obj1, startDt = "2017/01/01", endDt = "2017/02/01",  : 
#  ERROR.PWS info not provided  
```

The above error can be avoided by using the function in correct sequence as shown below.

```{r, eval = FALSE}
obj1 <- new("WGround", APIkey=APIkey, city="Denver", state="CO")
obj1 <- getPWSinfo(obj1)
obj1 <- getHistoricalWeather(obj1,startDt="2017/01/01",endDt="2017/01/02",verbose=F)
```

## Package in offline mode

The WGround based object can also be created in offline mode by passing the runmode parameter as offline. This mode is useful for learning about the package and running example. The user interface details are in next section.

```{r, eval=TRUE}
library(rpwsdata)
obj <- new("WGround", city="Fremont",state= "CA", runmode="offline")
obj <- getPWSinfo(obj,distance_mi = 10,maxPWScount = 3)
obj@selectedpws <- obj@pws[obj@pws$id == 'KCAFREMO83'|obj@pws$id == 'KCAFREMO88',]
obj <- CurrentWeatherbyPWS(obj)
Whistory <- getHistoricalWeather(obj,startDt="2017/01/01",endDt="2017/01/01",maxPWScount = 2)
```

## Interactive user interface

rpwsdata package comes with a Shiny based user interface. Shiny app can be called using function runShiny()

```{r, eval=FALSE}
runShiny()
```

There is an offline version of Shiny app as well and can be called using runShinyoffline()

```{r, eval=FALSE}
runShinyoffline()
```

## API Key

API key "71b9f1f144bb1e8c" can be used for evaluating only the package
