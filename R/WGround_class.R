#' WGround: Objects for representing weather data.
#' 
#' @description A class to query the weather data of a place from multiple Personal Weather Stations
#' @author Siddhartha Jetti - 50\% & Atul Lal - 50\% 
#' @name WGround-class
#' @import methods
#' @importFrom XML xmlTreeParse xmlToList
#' @importFrom xml2 read_xml
#' @importFrom jsonlite fromJSON
#' @importFrom utils head
#' @docType class
#'
#' @exportClass WGround
#' 
#' @slot APIkey       API key from Wunderground website
#' @slot city         A valid city name
#' @slot country      A valid country name
#' @slot state        A valid US state name
#' @slot pws          A data frame of all PWS
#' @slot selectedpws  A data frame of user selected PWS
#' @slot conditions   A data frame of current conditions for
#'                    selected PWS
#' @slot runmode      online or offline, default: online
#' 
setClass("WGround", 
         representation(APIkey="character",city="character",
                        country="character",state="character",pws="data.frame", 
                        conditions="data.frame", selectedpws="data.frame", runmode="character"), 
         prototype=prototype(APIkey="xxxxx",pws=data.frame(),conditions=data.frame(),selectedpws=data.frame(),runmode="online"))

#'
#' WGround class constructor function
#'
#' @param APIkey       API key from Wunderground
#' @param city         city
#' @param state        state or country for international city
#' @param runmode      online or offline, default: online
#' @export
#' @examples
#' obj <- new("WGround", city="Fremont",state= "CA",runmode="offline")
#'
WGround <- function(APIkey="",city="Fremont",state= "CA",runmode="offline") {
  ## Do other things if needed but return constructed object
  new("WGround", APIkey=APIkey, city = city, state = state, runmode = runmode)
}


#' The validity function for class WGround
#' @param object an object puportedly of class \code{WGround}
#' @return TRUE if valid, a character vector error messages if not
#' @keywords internal
#'
check_WGround <- function(object) {
  errors <- character()
 
  if(length(object@city) != 0) {
    if((length(object@state)==0) && (length(object@country)==0)){
      msg <- paste("Location information provided not complete. State or Country needed for entered city.")
      errors <- c(errors,msg)
    }
  }else{
    msg <- paste("Location information provided not complete. City needed to create the object.")
    errors <- c(errors,msg)
  }
  if((length(object@country)!= 0) && (length(object@city)!= 0) && (length(object@state)== 0)){
    if(toupper(object@country) %in% c("US","USA")){
    msg <- paste("For US cities enter state information.")
    errors <- c(errors,msg)
    }
  }
  if (length(errors) == 0) TRUE else errors
}
setValidity("WGround", check_WGround)

#' Generic Method for getPWSinfo defined
#'
#' @param object      the WGround object
#' @param verbose     mode
#' @param distance_mi distance from specified location
#' @param maxPWScount maximum number of PWS received
#' @rdname WGround-class
#' @export
#' @examples
#' obj <- new("WGround", city="Fremont",state= "CA",runmode="offline")
#' obj <- getPWSinfo(obj,verbose=FALSE)
#' 
setGeneric(name="getPWSinfo",
           def=function(object,verbose=TRUE,distance_mi=numeric(0),maxPWScount=numeric(0)){standardGeneric("getPWSinfo")})

#' Method of class WGround to get PWS data for a location
#' Return object with data frame with pws info. Throws an error if object is invalid.
#' @docType methods
#' @rdname WGround-class
#'
setMethod(f="getPWSinfo", 
          signature="WGround",
          definition=function(object,verbose=TRUE,distance_mi=numeric(0),maxPWScount=numeric(0)){
            if(class(object) == "WGround") {
              if (object@runmode == "online") {
                if((length(object@city)*length(object@state)==0) && (length(object@city)*length(object@country)==0)) {
                  stop('ERROR.Location info not complete !')
                }
                location <- ifelse(length(object@state)!=0,object@state,object@country)
                Url <- paste0("http://api.wunderground.com/api/", object@APIkey,"/geolookup/q/", location, "/", gsub(" ","_",object@city), ".xml")
                object@pws <- ParsePWSdata(Url)
                if(verbose){
                  cat("Accessing the PWS data from url : \n", Url)
                }
              } else if (object@runmode == "offline") {
                Url <- NULL
                object@pws <- ParsePWSdata(Url,local=T)
                if(verbose){
                  cat("Accessing the PWS data locally : \n")
                }
              }
              npws <- length(unique(object@pws$id))
              if(length(distance_mi)!=0){object@pws <- object@pws[object@pws$distance_mi<=distance_mi,]}
              if(length(maxPWScount)!=0){object@pws <- object@pws[1:maxPWScount,]}
              if(verbose){
                cat("\n",npws," PWS Stations obtained for the requested location !\n")
                if(length(distance_mi)!=0){cat("\nIncluding only PWS Stations within ",distance_mi," miles of requested location.\n")}
                if(length(maxPWScount)!=0){cat("\nIncluding only ",maxPWScount," PWS Stations for the requested location. \n")}
                print(head(object@pws))
              }
              return(object)
            }
            else {
              stop("Object provided is not of required Type !")
            }
          })


#' Generic Method for CurrentWeatherbyPWS defined
#'
#' @rdname WGround-class
#' @export
#' @examples
#' obj@selectedpws <- obj@pws[obj@pws$id == 'KCAFREMO83'|obj@pws$id == 'KCAFREMO88',]
#' obj <- CurrentWeatherbyPWS(obj,verbose=FALSE)
#' 
setGeneric(name="CurrentWeatherbyPWS", 
           def=function(object,verbose=TRUE) {standardGeneric("CurrentWeatherbyPWS")})

#' Method of class WGround to get Current weather conditions
#' Return object with data frame with conditions. Throws an error if object is invalid.
#' @docType methods
#' @rdname WGround-class
#'
setMethod(f="CurrentWeatherbyPWS", 
          signature="WGround", 
          definition=function(object,verbose=TRUE){
            if(class(object) == "WGround"){
              if(nrow(object@pws) == 0) {
                stop('ERROR.PWS info not provided')
              }
              object@conditions <- data.frame()
              
              if(nrow(object@selectedpws)==0){
                pws_req <- head(object@pws,3)
              }else{
                pws_req <- object@selectedpws
              }
              
              for(i in 1:nrow(pws_req)) {    
                Url <- paste("http://api.wunderground.com/api/", object@APIkey,"/conditions/q/pws:", as.character(pws_req[i,"id"]),
                             ".json", sep = "")
                if(object@runmode=="online"){
                  if(verbose){cat("\nAccessing the PWS data from url : \n ", Url, sep="")}
                  object@conditions <- rbind(object@conditions, Parseconditiondata(Url),stringsAsFactors=FALSE)
                } else if(object@runmode=="offline"){
                  if(verbose){cat("\nAccessing Local JSON files \n ")}
                  object@conditions <- rbind(object@conditions, Parseconditiondata(Url,local=T),stringsAsFactors=FALSE)
                  
                }
             }
              if(verbose){cat("\n\n");print(object@conditions)}
              return(object)
            }
            else {
              stop("Object provided is not of required Type !")
            }
          })



#' Generic Method getHistoricalWeather defined
#'
#' @param startDt     start date for historical data
#' @param endDt       end date for historical data
#' @rdname WGround-class
#' @export
#' @examples
#' df_hist <- getHistoricalWeather(obj,verbose=FALSE,startDt="2017/01/01",endDt="2017/01/01")
#' head(df_hist,5)
#' 
setGeneric(name="getHistoricalWeather", 
           def=function(object,startDt,endDt,maxPWScount=3,verbose=TRUE) {standardGeneric("getHistoricalWeather")})

#' Method of class WGround to get Historical weather data
#' Return data frame with historical weather data. Throws an error if object is invalid.
#' @docType methods
#' @rdname WGround-class
#'
setMethod(f="getHistoricalWeather", 
          signature="WGround", 
          definition=function(object,startDt,endDt,maxPWScount=3,verbose=TRUE){
            if(class(object) == "WGround"){
              if(nrow(object@pws) == 0) {
                stop('ERROR.PWS info not provided')
              }
              
              if(nrow(object@selectedpws)==0){
                pws_req <- object@pws
              }else{
                pws_req <- object@selectedpws
              }
              
              
              startDt <- gsub("-|/","/",startDt)
              endDt <- gsub("-|/","/",endDt)
              
              dates <- seq(from=as.Date(startDt,format="%Y/%m/%d"),to=as.Date(endDt,format=  "%Y/%m/%d"), by=1)
              dates <- as.character(dates)
              dfByDate <- NULL
              
              ## Loop through the dates
              for(i in 1:length(dates)){
                j <- gsub("-","",dates[i])
                dfByPWS <- NULL
                if(verbose){cat("\nAccessing the Weather for date : ", dates[i])}
                
                ## Loop through PWS stations for requested location
                indx <- pmin(maxPWScount,nrow(pws_req))
                for(pwsID in pws_req$id[1:indx]){
                  URL <- paste0("http://api.wunderground.com/api/",object@APIkey,"/history_",j,"/q/pws:",pwsID,".xml")
                  if(object@runmode=="online"){
                    dailydata <- ParseHistoricalWeatherdata(URL,j,pwsID,verbose)
                  }else if(object@runmode=="offline"){
                    cat("\nAccessing the weather data locally : \n")
                    dailydata <- ParseHistoricalWeatherdata(URL,j,pwsID,verbose, local=T)
                  }
                  dfByPWS <- rbind(dfByPWS,dailydata)
                }
                dfByDate <- rbind(dfByDate,dfByPWS)
                
              }
              if(verbose){print(head(dfByDate))}
              return(dfByDate)
            }
            else {
              stop("Object provided is not of required Type !")
            }
          })

#' A function to get geo data from wunderground
#' gets called from getPWSinfo method
#' Return data frame with PWS info. Throws an error if invalid input.
#' @param URL     fully formed url for API call
#' @param local   set to F for online mode otherwise T
#' @return pws_df data frame with selected columns
#'
ParsePWSdata <- function(URL, local=F) {
  
  if(local){
    tree <- xml2::read_xml(system.file("extdata","PWS_data.xml", package = "rpwsdata")) %>%
      XML::xmlTreeParse()
    
  }else{
    ## create a connection
    con <- url(URL)
    xmlText <- readLines(con)
    close(con)
    
    ## Parse the XML text
    tree <- XML::xmlTreeParse(xmlText)
  }
  
  tree_list <- XML::xmlToList(tree)
  pws_list <-  tree_list$location$nearby_weather_stations$pws
  
  if(length(pws_list) == 0){ stop(" ERROR.Enter Valid description for City/State/Country !") }
  info <- row.names(pws_list)
  checkNULL <- unlist(lapply(pws_list,is.null))
  
  if(any(checkNULL)){pws_list[which(checkNULL)] <- NA}
  
  pws_vec <- unlist(pws_list)
  pws_df <- as.data.frame(t(matrix(pws_vec,nrow=length(info))),stringsAsFactors =F)
  names(pws_df) <- info
  pws_df$lat <-as.numeric(pws_df$lat)
  pws_df$lon <-as.numeric(pws_df$lon)
  
  ## return the required pws data frame
  pws_df <- pws_df[, c("id","neighborhood","city","state","country","lat","lon","distance_mi")]
  return(pws_df)
}


#' A function to get current conditions data from wunderground
#' gets called from CurrentWeatherbyPWS method
#' Return list with conditions info. Throws an error if invalid input.
#' @param url     fully formed url for API call
#' @param local   set to F for online mode otherwise T
#' @return conditiondata list with selected elements
#'
Parseconditiondata <- function(url, local=F) {
  conditiondata <- tryCatch({
    if(local){
      url <- gsub(":","_",substr(url,nchar(url)-18,nchar(url)))
      url <- system.file("extdata",url, package = "rpwsdata")
      jsonlite::fromJSON(url)
    }else{
      jsonlite::fromJSON(url)
    }
    
  }, error = function(x) {
    message(paste("URL does not seem to exist:", url))
    message("The original error message:")
    message(x)
    return(NA)
  }, warning = function(x) {
    message(paste("URL caused a warning:", url))
    message("Here's the original warning message:")
    message(x)
    return(NA)
  }, finally = )
  conditiondata <- conditiondata$current_observation[c('station_id','weather','temp_f','relative_humidity','wind_mph','pressure_in','windchill_f','feelslike_f','visibility_mi','precip_today_in')]
  return(conditiondata)
}

#' A function to get historical weather data from wunderground
#' gets called from getHistoricalWeather method
#' Return data frame with historical weather data. Throws an error if query is unsuccessful.
#' @param URL       fully formed url for API call
#' @param Dt        date of historical data
#' @param PWSid     pws_id for the station
#' @param verbose   shows details on process
#' @param local     set to F for online mode otherwise T
#' @return wdata_df data frame with selected columns
#'
ParseHistoricalWeatherdata <- function(URL,Dt,PWSid,verbose, local=F){
 
  if(local){
    file_name <- paste0("history_",Dt,"_",PWSid,".xml")
    tree <- xml2::read_xml(system.file("extdata",file_name, package = "rpwsdata")) %>%
      XML::xmlTreeParse()
    
  }else{
    ## create a connection
    if(verbose){cat("\nAccessing the Weather data from url : \n", URL)}
    con <- url(URL)
    xmlText <- readLines(con)
    close(con)
    
    ## Parse the XML text
    tree <- XML::xmlTreeParse(xmlText)
  }
  
  tree_list <- XML::xmlToList(tree)
  wdata_list <-  tree_list$history$observations

  ## Check if data pull is successful
  if(is.null(wdata_list) || length(wdata_list)==0){
    if(verbose){cat("\nData Pull from url : FAILED\n")}
    return(NULL)
  } else{
    if(verbose){cat("\nData Pull from url : SUCCESSFUL\n")}
    ## Fill NAs for NULL elements
    checkNULL <- unlist(lapply(wdata_list,is.null))
    if(any(checkNULL)){wdata_list[which(checkNULL)] <- NA}
    
    ## Convert into correct format
    wdata_matrix <- as.matrix(wdata_list)
    required_metrics <- c("tempm","tempi","dewptm","dewpti","hum","pressurem","pressurei")
    time_list <- wdata_matrix["date",]
    wdata_vec <- unlist(wdata_matrix[required_metrics,])
    wdata_df <- as.data.frame(t(matrix(wdata_vec,nrow = length(required_metrics))))
    names(wdata_df) <- required_metrics
    
    time <- character()
    for(i in 1:nrow(wdata_df)){
      time <- c(time,time_list[[i]]$pretty)
    }
    
    wdata_df <- cbind(wdata_df,time)
    wdata_df$Date <- Dt
    wdata_df$ID <- PWSid
    wdata_df$time <- substr(wdata_df$time,1,12)
    wdata_df <- wdata_df[,c("Date","time","ID",required_metrics)]
    wdata_df[, required_metrics] <- sapply(wdata_df[, required_metrics], as.numeric)
    return(wdata_df)
  }
  
}

################################ For testing purposes ########################################
# obj <- new("WGround", city="Fremont",state= "CA", runmode="offline")
# obj <- getPWSinfo(obj,distance_mi = 10,maxPWScount = 3)
# obj@selectedpws <- obj@pws[obj@pws$id == 'KCAFREMO83'|obj@pws$id == 'KCAFREMO88',]
# obj <- CurrentWeatherbyPWS(obj)
# Whistory <- getHistoricalWeather(obj,startDt="2017/01/01",endDt="2017/01/01",maxPWScount = 2)
# obj1 <- new("WGround", city="Fremont",state= "CA")
# obj1 <- getPWSinfo(obj1,distance_mi = 10,maxPWScount = 3)
# obj1 <- CurrentWeatherbyPWS(obj1)
# Whistory1 <- getHistoricalWeather(obj1,startDt="2017/01/01",endDt="2017/01/01",maxPWScount = 2)
#############################################################################################