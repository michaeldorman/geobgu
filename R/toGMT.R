
toGMT = function(time) {
  as.POSIXct(format.POSIXct(time_local, tz = "GMT"), tz = "GMT")
}
