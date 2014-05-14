#' Calculate piecewise distance between successive locations
#' 
#'  ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#'  ~~ If necessary, more details than the description above ~~
#' 
#' @param xy  ~~Describe \code{xy} here~~
#' @param Time  ~~Describe \code{Time} here~~
#' @param great.circle  ~~Describe \code{great.circle} here~~
#' @param R  ~~Describe \code{R} here~~
#' @return  ~Describe the value returned  If it is a LIST, use 
#' \item{comp1 }{Description of 'comp1'}  \item{comp2 }{Description of
#' 'comp2'}  ...
#' @note  ~~further notes~~
#' @author  Devin S. Johnson
#' @export
#' 
pwSpeed <-
function(xy, Time, great.circle, R=6378.137) {
    tripDist <- pwDistance(xy, great.circle=great.circle, R=R)/c(1,diff(Time))
}

