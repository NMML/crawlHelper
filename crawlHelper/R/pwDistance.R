#' Calculate distance between successive points
#' 
#'  ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#'  ~~ If necessary, more details than the description above ~~
#' 
#' @param xy  ~~Describe \code{xy} here~~
#' @param great.circle  ~~Describe \code{great.circle} here~~
#' @param R  ~~Describe \code{R} here~~
#' @return  ~Describe the value returned  If it is a LIST, use 
#' @note  ~~further notes~~
#' @author  ~~who you are~~
#' @export
#' 
pwDistance <-
function(xy, great.circle, R=6378.137) {
  n <- nrow(xy)
  y1 <- xy[,2]
  x1 <- xy[,1]
  y2 <- c(y1[1],y1[-n])
  x2 <- c(x1[1],x1[-n])    
  if(great.circle) {
      y1 <- y1*pi/180
      x1 <- x1*pi/180
      y2 <- y2*pi/180
      x2 <- x2*pi/180
      dx <- x2-x1
      num <- (cos(y2)*sin(dx))^2 + (cos(y1)*sin(y2)-sin(y1)*cos(y2)*cos(dx))^2
      den <- sin(y1)*sin(y2) + cos(y1)*cos(y2)*cos(dx)
      return(R*atan2(sqrt(num), den))
  }
  else {
    return(sqrt((y1-y2)^2 + (x1-x2)^2))
  }
}

