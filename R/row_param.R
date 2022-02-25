#' @export

row_param <- function(file, dist) {         # requires filepath (.txt output from Decision Support) and "dist", which is the numerical distance value (i.e. 1600)
  if(missing(dist)) {
    dist <- 1400
    header <- read.csv(file, nrows = 2, header = FALSE)
    header_names <- sapply(header, paste, collapse ="_")
    header_names2 <- make.names(header_names[-33], unique = TRUE)
    header_names3 <- gsub("\\.", "", header_names2)
    header_names4 <- gsub("X", "", header_names3)
    df <- read.csv(file, skip = 2, header = FALSE)
    names(df) <- header_names4
    df$Angle.offset <- ifelse((df$Id_ == "L"), (df$Bearing_deg-90)%%360, (df$Bearing_deg+90)%%360)
    df$Radians.offset <- df$Angle.offset*pi/180
    df$Distance <- as.numeric(dist)/100000000
    df$Sin.Radians.offset <- sin(df$Radians.offset)
    df$Departure <- df$Distance*df$Sin.Radians.offset
    df$Cos.Radians.offset <- cos(df$Radians.offset)
    df$Latitude_correction <- df$Distance*df$Cos.Radians.offset
    df$lat_corrd <- df$Latitude_deg + df$Latitude_correction
    df$lon_corrd <- df$Longitude_deg + df$Departure
    warning("No distance specified, default of 1400 used")
    return(df)
  }
  else {
    header <- read.csv(file, nrows = 2, header = FALSE)
    header_names <- sapply(header, paste, collapse ="_")
    header_names2 <- make.names(header_names[-33], unique = TRUE)
    header_names3 <- gsub("\\.", "", header_names2)
    header_names4 <- gsub("X", "", header_names3)
    df <- read.csv(file, skip = 2, header = FALSE)
    names(df) <- header_names4
    df$Angle.offset <- ifelse((df$Id_ == "L"), (df$Bearing_deg-90)%%360, (df$Bearing_deg+90)%%360)
    df$Radians.offset <- df$Angle.offset*pi/180
    df$Distance <- as.numeric(dist)/100000000
    df$Sin.Radians.offset <- sin(df$Radians.offset)
    df$Departure <- df$Distance*df$Sin.Radians.offset
    df$Cos.Radians.offset <- cos(df$Radians.offset)
    df$Latitude_correction <- df$Distance*df$Cos.Radians.offset
    df$lat_corrd <- df$Latitude_deg + df$Latitude_correction
    df$lon_corrd <- df$Longitude_deg + df$Departure
    return(df)
  }
}
