#' @import maptools
getIndivPropDaylight = function(x, y, dt.start, dt.end, solarDep){
  times=seq(dt.start, dt.end, 60)
  sum(maptools::solarpos(cbind(rep(x, length(times)),rep(y,length(times))), times)[,2]>-solarDep)/length(times)
}


#' Calculate proportion of daylight with in a period of time
#' 
#'  ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#'  ~~ If necessary, more details than the description above ~~
#' 
#' @param xy  ~~Describe \code{xy} here~~
#' @param dateTime.start  ~~Describe \code{dateTime.start} here~~
#' @param dateTime.end  ~~Describe \code{dateTime.end} here~~
#' @param delta  ~~Describe \code{delta} here~~
#' @param solarDep  ~~Describe \code{solarDep} here~~
#' @return  ~Describe the value returned  If it is a LIST, use 
#' @author Devin S. Johnson
#' @export
#' 
propDaylight <-
  function(xy, dateTime.start, dateTime.end, delta, solarDep=12){
    if(missing(dateTime.end) & missing(delta)){
      dateTime.end <- dateTime.start + c(diff(as.numeric(dateTime.start)), 3600)
    }
    if(missing(dateTime.end) & !missing(delta)) dateTime.end <- dateTime.start + delta*3600
    mapply(getIndivPropDaylight, x=xy[,1], y=xy[,2], dt.start=dateTime.start, dt.end=dateTime.end, solarDep=solarDep)
  }


# propDaylight <-
# function(xy, dateTime.start, dateTime.end, delta, solarDep=12){
# 	if(missing(dateTime.end) & missing(delta)){
# 		dateTime.end <- dateTime.start + c(diff(as.numeric(dateTime.start)), 3600)
# 	}
# 	if(missing(dateTime.end) & !missing(delta)) dateTime.end <- dateTime.start + delta*3600
# 	Epoch <- as.POSIXct(strptime("1970-01-01 00:00:00", "%Y-%m-%d %H:%M:%S",tz='GMT'),tz='GMT')
# 	dusk0.end <- prev.crepuscule(xy, dateTime.end, solarDep=solarDep, direction="dusk")
# 	dusk1.start <- next.crepuscule(xy, dateTime.start, solarDep=solarDep, direction="dusk")
# 	dusk1.end <- next.crepuscule(xy, dateTime.end, solarDep=solarDep, direction="dusk")
# 	dawn1.start <- next.crepuscule(xy, dateTime.start, solarDep=solarDep, direction="dawn")
# 	dawn0.end <- prev.crepuscule(xy, dateTime.end, solarDep=solarDep, direction="dawn")
# 	day.start <- sunFuncs:::solarpos(xy, dateTime.start)[,2] > -solarDep
# 	day.end <- sunFuncs:::solarpos(xy, dateTime.end)[,2] > -solarDep	
# 	c1 <- ifelse(day.start & day.end & dusk1.start==dusk1.end, as.numeric(dateTime.end)-as.numeric(dateTime.start), 0)
# 	c2 <- ifelse(day.start & (!day.end) & dusk1.start==dusk0.end, as.numeric(dusk1.start)-as.numeric(dateTime.start), 0)
# 	c3 <- ifelse(day.start & day.end & dawn1.start==dawn0.end, (as.numeric(dusk1.start)-as.numeric(dateTime.start)) + 
# 					                                           (as.numeric(dateTime.end)-as.numeric(dawn0.end)), 0)
# 	c4 <- ifelse((!day.start) & day.end & dawn1.start==dawn0.end, as.numeric(dateTime.end)-as.numeric(dawn1.start), 0)
# 	c5 <- ifelse((!day.start) & (!day.end) & dawn1.start==dawn0.end, (as.numeric(dawn1.start)-as.numeric(dateTime.start)) + 
# 					                                                 (as.numeric(dateTime.end)-as.numeric(dawn0.end)), 0)
# 	tdiff <- as.numeric(dateTime.end)-as.numeric(dateTime.start)
# 	return(ifelse(tdiff<=24*3600, (c1+c2+c3+c4+c5)/tdiff, NA))
# }

