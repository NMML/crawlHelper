getPixTemplate <-
function(x){
	if(!(inherits(x,"SpatialPixels") | inherits(x,"SpatialGrid"))) stop("Argument must be a 'SpatialPixels' or 'SpatialGrid' object!")
	firstCell <- as.vector(x@grid@cellcentre.offset)
	eps <- as.vector(x@grid@cellsize)
	dims <- as.vector(x@grid@cells.dim)
	x.center <- seq(from=firstCell[1], by=eps[1], length=dims[1])
	y.center <- seq(from=firstCell[2], by=eps[2], length=dims[2])
	return(list(x=x.center, y=y.center))
}

