% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WGround_class.R
\docType{class}
\name{WGround-class}
\alias{WGround-class}
\alias{getPWSinfo}
\alias{getPWSinfo,WGround-method}
\alias{CurrentWeatherbyPWS}
\alias{CurrentWeatherbyPWS,WGround-method}
\alias{getHistoricalWeather}
\alias{getHistoricalWeather,WGround-method}
\title{WGround: Objects for representing weather data.}
\usage{
getPWSinfo(object, verbose = TRUE, distance_mi = numeric(0),
  maxPWScount = numeric(0))

\S4method{getPWSinfo}{WGround}(object, verbose = TRUE,
  distance_mi = numeric(0), maxPWScount = numeric(0))

CurrentWeatherbyPWS(object, verbose = TRUE)

\S4method{CurrentWeatherbyPWS}{WGround}(object, verbose = TRUE)

getHistoricalWeather(object, startDt, endDt, maxPWScount = 3,
  verbose = TRUE)

\S4method{getHistoricalWeather}{WGround}(object, startDt, endDt,
  maxPWScount = 3, verbose = TRUE)
}
\arguments{
\item{object}{the WGround object}

\item{verbose}{mode}

\item{distance_mi}{distance from specified location}

\item{maxPWScount}{maximum number of PWS received}

\item{startDt}{start date for historical data}

\item{endDt}{end date for historical data}
}
\description{
A class to query the weather data of a place from multiple Personal Weather Stations

Generic Method for getPWSinfo defined

Method of class WGround to get PWS data for a location
Return object with data frame with pws info. Throws an error if object is invalid.

Generic Method for CurrentWeatherbyPWS defined

Method of class WGround to get Current weather conditions
Return object with data frame with conditions. Throws an error if object is invalid.

Generic Method getHistoricalWeather defined

Method of class WGround to get Historical weather data
Return data frame with historical weather data. Throws an error if object is invalid.
}
\section{Slots}{

\describe{
\item{\code{APIkey}}{API key from Wunderground website}

\item{\code{city}}{A valid city name}

\item{\code{country}}{A valid country name}

\item{\code{state}}{A valid US state name}

\item{\code{pws}}{A data frame of all PWS}

\item{\code{selectedpws}}{A data frame of user selected PWS}

\item{\code{conditions}}{A data frame of current conditions for
selected PWS}

\item{\code{runmode}}{online or offline, default: online}
}}

\examples{
obj <- new("WGround", city="Fremont",state= "CA",runmode="offline")
obj <- getPWSinfo(obj,verbose=FALSE)

obj@selectedpws <- obj@pws[obj@pws$id == 'KCAFREMO83'|obj@pws$id == 'KCAFREMO88',]
obj <- CurrentWeatherbyPWS(obj,verbose=FALSE)

df_hist <- getHistoricalWeather(obj,verbose=FALSE,startDt="2017/01/01",endDt="2017/01/01")
head(df_hist,5)

}
\author{
Siddhartha Jetti - 50\% & Atul Lal - 50\%
}
