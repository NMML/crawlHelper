netDistance <-
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

