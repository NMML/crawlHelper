\name{countPointsInGrid}
\alias{countPointsInGrid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Count number of points in each cell of a spatial grid 
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
countPointsInGrid(xy.points, xy.centers = NULL, template = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xy.points}{
%%     ~~Describe \code{xy.points} here~~
}
  \item{xy.centers}{
%%     ~~Describe \code{xy.centers} here~~
}
  \item{template}{
%%     ~~Describe \code{template} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
