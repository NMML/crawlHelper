\name{netDistance}
\alias{netDistance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculate distance from a fixed point
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
netDistance(xy, xyFix, great.circle, R = 6378.137)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xy}{
%%     ~~Describe \code{xy} here~~
}
  \item{xyFix}{
%%     ~~Describe \code{xyFix} here~~
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
function(xy, xyFix, great.circle, R=6378.137) {
  if(missing(xyFix)) xyFix <- xy[1,]
  y1 <- xy[,2]
  x1 <- xy[,1]
  y2 <- xyFix[2]
  x2 <- xyFix[1]    
  if(great.circle) {
      y1 <- y1*pi/180
      x1 <- x1*pi/180
      y2 <- y2*pi/180
      x2 <- x2*pi/180
      dx <- x1-x2
      num <- (cos(y2)*sin(dx))^2 + (cos(y1)*sin(y2)-sin(y1)*cos(y2)*cos(dx))^2
      den <- sin(y1)*sin(y2) + cos(y1)*cos(y2)*cos(dx)
      return(R*atan2(sqrt(num), den))
  }
  else {
    return(sqrt((y1-y2)^2 + (x1-x2)^2))
  }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
