\name{propDaylight}
\alias{propDaylight}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate proportion of daylight with in a period of time
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
propDaylight(xy, dateTime.start, dateTime.end, delta, solarDep = 12)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xy}{
%%     ~~Describe \code{xy} here~~
}
  \item{dateTime.start}{
%%     ~~Describe \code{dateTime.start} here~~
}
  \item{dateTime.end}{
%%     ~~Describe \code{dateTime.end} here~~
}
  \item{delta}{
%%     ~~Describe \code{delta} here~~
}
  \item{solarDep}{
%%     ~~Describe \code{solarDep} here~~
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

dateTime.start <- as.POSIXlt(Sys.time(), tz="GMT") + c(0:100)*3600
dateTime.end <- dateTime.start + 6*3600
# Seattle, WA Lon/Lat
xy <- cbind(rep(-122.3308333, length(dateTime.start)), rep(47.6063889, length(dateTime.start)))
propDaylight(xy, dateTime.start, dateTime.end)



}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
