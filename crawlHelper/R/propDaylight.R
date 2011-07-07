propDaylight <-
function(xy, dateTime.start, dateTime.end, delta, solarDep=12){
	if(missing(dateTime.end) & missing(delta)){
		dateTime.end <- dateTime.start + c(diff(as.numeric(dateTime.start)), 3600)
	}
	if(missing(dateTime.end) & !missing(delta)) dateTime.end <- dateTime.start + delta*3600
	Epoch <- as.POSIXct(strptime("1970-01-01 00:00:00", "%Y-%m-%d %H:%M:%S",tz='GMT'),tz='GMT')
	dusk0.end <- prev.crepuscule(xy, dateTime.end, solarDep=solarDep, direction="dusk")
	dusk1.start <- next.crepuscule(xy, dateTime.start, solarDep=solarDep, direction="dusk")
	dusk1.end <- next.crepuscule(xy, dateTime.end, solarDep=solarDep, direction="dusk")
	dawn1.start <- next.crepuscule(xy, dateTime.start, solarDep=solarDep, direction="dawn")
	dawn0.end <- prev.crepuscule(xy, dateTime.end, solarDep=solarDep, direction="dawn")
	day.start <- solarpos(xy, dateTime.start)[,2] > -solarDep
	day.end <- solarpos(xy, dateTime.end)[,2] > -solarDep	
	c1 <- ifelse(day.start & day.end & dusk1.start==dusk1.end, as.numeric(dateTime.end)-as.numeric(dateTime.start), 0)
	c2 <- ifelse(day.start & (!day.end) & dusk1.start==dusk0.end, as.numeric(dusk1.start)-as.numeric(dateTime.start), 0)
	c3 <- ifelse(day.start & day.end & dawn1.start==dawn0.end, (as.numeric(dusk1.start)-as.numeric(dateTime.start)) + 
					                                           (as.numeric(dateTime.end)-as.numeric(dawn0.end)), 0)
	c4 <- ifelse((!day.start) & day.end & dawn1.start==dawn0.end, as.numeric(dateTime.end)-as.numeric(dawn1.start), 0)
	c5 <- ifelse((!day.start) & (!day.end) & dawn1.start==dawn0.end, (as.numeric(dawn1.start)-as.numeric(dateTime.start)) + 
					                                                 (as.numeric(dateTime.end)-as.numeric(dawn0.end)), 0)
	tdiff <- as.numeric(dateTime.end)-as.numeric(dateTime.start)
	return(ifelse(tdiff<=24*3600, (c1+c2+c3+c4+c5)/tdiff, NA))
}

