\name{pwBearing}
\alias{pwBearing}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate great circle bearings for each location
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pwBearing(xy)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xy}{
%%     ~~Describe \code{xy} here~~
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
function(xy) {
    n <- nrow(xy)
    y1 <- xy[,2]*pi/180
    x1 <- xy[,1]*pi/180
    y2 <- c(y1[-1],NA)
    x2 <- c(x1[-1],NA)
    dx <- x2-x1
    theta <- 180*atan2(sin(dx)*cos(y2), cos(y1)*sin(y2)-sin(y1)*cos(y2)*cos(dx))/pi
    (360 + theta) \%\% 360
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
