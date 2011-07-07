pwSpeed <-
function(xy, Time, great.circle, R=6378.137) {
    tripDist <- pwDistance(xy, great.circle=great.circle, R=R)/c(1,diff(Time))
}

