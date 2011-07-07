\name{pwSpeed}
\alias{pwSpeed}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate piecewise distance between successive locations
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
pwSpeed(xy, Time, great.circle, R = 6378.137)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xy}{
%%     ~~Describe \code{xy} here~~
}
  \item{Time}{
%%     ~~Describe \code{Time} here~~
}
  \item{great.circle}{
%%     ~~Describe \code{great.circle} here~~
}
  \item{R}{
%%     ~~Describe \code{R} here~~
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
function(xy, Time, great.circle, R=6378.137) {
    tripDist <- pwDistance(xy, great.circle=great.circle, R=R)/c(1,diff(Time))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
