#' Calculate great circle bearings for each location
#' 
#'  ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#'  ~~ If necessary, more details than the description above ~~
#' 
#' @param xy  ~~Describe \code{xy} here~~
#' @return  ~Describe the value returned  If it is a LIST, use 
#' @note  ~~further notes~~
#' @author  Devin S. Johnson
#' @export
#' 
pwBearing <-
function(xy) {
    n <- nrow(xy)
    y1 <- xy[,2]*pi/180
    x1 <- xy[,1]*pi/180
    y2 <- c(y1[-1],NA)
    x2 <- c(x1[-1],NA)
    dx <- x2-x1
    theta <- 180*atan2(sin(dx)*cos(y2), cos(y1)*sin(y2)-sin(y1)*cos(y2)*cos(dx))/pi
    (360 + theta) %% 360
}

