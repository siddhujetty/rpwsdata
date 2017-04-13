## ---- eval=FALSE---------------------------------------------------------
#  #library(rpwsdata)
#  obj <- new("WGround", APIkey=APIkey, city="San Francisco",state= "CA",country="USA")
#  obj <- new("WGround", APIkey=APIkey, city="San Francisco",state= "CA")

## ---- eval=FALSE---------------------------------------------------------
#  obj <- new("WGround", APIkey=APIkey, city="San Francisco")
#  #Error in validObject(.Object) :
#  #  invalid class "WGround" object: Location information provided not complete. State or Country needed for entered city.
#  
#  obj <- new("WGround", APIkey=APIkey, city="San Francisco", country="US")
#  #Error in validObject(.Object) :
#  #  invalid class "WGround" object: For US cities enter state information.

## ---- eval=FALSE---------------------------------------------------------
#  obj <- new("WGround", APIkey=APIkey, city="Paris",country="France")

## ---- eval=FALSE---------------------------------------------------------
#  obj <- getPWSinfo(obj)

## ---- eval = FALSE-------------------------------------------------------
#  obj <- getPWSinfo(obj, verbose = F, distance_mi = 0.5, maxPWScount = 3)

## ---- eval = FALSE-------------------------------------------------------
#  obj <- CurrentWeatherbyPWS(obj)
#  obj1 <- CurrentWeatherbyPWS(obj, verbose = F)

## ---- eval = FALSE-------------------------------------------------------
#  obj1 <- new("WGround", APIkey=APIkey, city="Denver", state="CO")
#  obj1 <- CurrentWeatherbyPWS(obj1)
#  #Error in CurrentWeatherbyPWS(obj) : ERROR.PWS info not provided

## ---- eval = FALSE-------------------------------------------------------
#  obj1 <- new("WGround", APIkey=APIkey, city="Denver", state="CO")
#  obj1 <- getPWSinfo(obj1)
#  obj1 <- CurrentWeatherbyPWS(obj1)

## ---- eval = FALSE-------------------------------------------------------
#  ## startDt and endDt in yyyy/mm/dd format
#  weather_hist <- getHistoricalWeather(obj,startDt="2017/01/01",endDt="2017/01/02",maxPWScount=1)

## ---- eval = FALSE-------------------------------------------------------
#  obj1 <- new("WGround", APIkey=APIkey, city="Denver", state="CO")
#  obj1 <- getHistoricalWeather(obj1,startDt="2017/01/01",endDt="2017/01/02",maxPWScount=1,verbose=F)
#  #Error in getHistoricalWeather(obj1, startDt = "2017/01/01", endDt = "2017/02/01",  :
#  #  ERROR.PWS info not provided

## ---- eval = FALSE-------------------------------------------------------
#  obj1 <- new("WGround", APIkey=APIkey, city="Denver", state="CO")
#  obj1 <- getPWSinfo(obj1)
#  obj1 <- getHistoricalWeather(obj1,startDt="2017/01/01",endDt="2017/01/02",verbose=F)

## ---- eval=TRUE----------------------------------------------------------
library(rpwsdata)
obj <- new("WGround", city="Fremont",state= "CA", runmode="offline")
obj <- getPWSinfo(obj,distance_mi = 10,maxPWScount = 3)
obj@selectedpws <- obj@pws[obj@pws$id == 'KCAFREMO83'|obj@pws$id == 'KCAFREMO88',]
obj <- CurrentWeatherbyPWS(obj)
Whistory <- getHistoricalWeather(obj,startDt="2017/01/01",endDt="2017/01/01",maxPWScount = 2)

## ---- eval=FALSE---------------------------------------------------------
#  runShiny()

## ---- eval=FALSE---------------------------------------------------------
#  runShinyoffline()

