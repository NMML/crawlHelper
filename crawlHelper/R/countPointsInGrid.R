countPointsInGrid <-
function(xy.points, xy.centers=NULL, template=NULL){
	if(is.null(xy.centers) & is.null(template)) stop("At least one of 'xy.centers' or 'template' must be specified!")
	if(!is.null(template)) xy.centers <- getPixTemplate(template)
	x <- sort(xy.centers$x)
	y <- sort(xy.centers$y)
	x.pts <- xy.points[,1]
	y.pts <- xy.points[,2]
	eps.x <- min(diff(x))
	eps.y <- min(diff(y))
	x.cut <- c(x[1]-eps.x/2, x+eps.x/2)
	y.cut <- c(y[1]-eps.y/2, y+eps.y/2)
	mat <- matrix(0,length(y),length(x))
	x.int <- findInterval(x.pts, x.cut)
	y.int <- findInterval(y.pts, y.cut)
	for(i in 1:length(x.pts)){
		mat[length(y)-y.int[i]+1, x.int[i]] <- mat[length(y)-y.int[i]+1, x.int[i]] + 1
	}
	return(list(M=mat,x=x,y=y))
}

