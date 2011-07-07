\name{getPixTemplate}
\alias{getPixTemplate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Make a grid template from a set of pixel centers
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getPixTemplate(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
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
function(x){
	if(!(inherits(x,"SpatialPixels") | inherits(x,"SpatialGrid"))) stop("Argument must be a 'SpatialPixels' or 'SpatialGrid' object!")
	firstCell <- as.vector(x@grid@cellcentre.offset)
	eps <- as.vector(x@grid@cellsize)
	dims <- as.vector(x@grid@cells.dim)
	x.center <- seq(from=firstCell[1], by=eps[1], length=dims[1])
	y.center <- seq(from=firstCell[2], by=eps[2], length=dims[2])
	return(list(x=x.center, y=y.center))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
