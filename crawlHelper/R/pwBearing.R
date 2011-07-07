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

