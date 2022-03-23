#' row_param
#'
#' This function takes the output from the bespoke "New Decision" software
#' and calculates corrections.
#'
#' @export

row_param <- function(file, dist) {         # requires filepath (.txt output from Decision Support) and "dist", which is the numerical distance value (i.e. 1600)
    
  header <- read.csv(file, nrows = 2, header = FALSE)
  header_names <- sapply(header, paste, collapse ="_")
  header_names2 <- gsub("\\.", "", header_names)
  header_names3 <- gsub("\\s+", "", header_names2)
  header_names4 <- gsub("X", "", header_names3)
  header_names5 <- gsub("_$", "", header_names4)
  header_names6 <- gsub("^_", "", header_names5)
  header_names7 <- make.names(header_names6[-33], unique = TRUE)
  df <- read.csv(file, skip = 2, header = FALSE)
  names(df) <- header_names7
  if(missing(dist)) {
    dist <- df$Row_m[1]*500
    df$Angle.offset <- ifelse((df$Id_ == "L"), (df$Bearing_deg-90)%%360, (df$Bearing_deg+90)%%360)
    df$Radians.offset <- df$Angle.offset*pi/180
    df$Distance <- as.numeric(dist)/100000000
    df$Sin.Radians.offset <- sin(df$Radians.offset)
    df$Departure <- df$Distance*df$Sin.Radians.offset
    df$Cos.Radians.offset <- cos(df$Radians.offset)
    df$Latitude_correction <- df$Distance*df$Cos.Radians.offset
    df$lat_corrd <- df$Latitude_deg + df$Latitude_correction
    df$lon_corrd <- df$Longitude_deg + df$Departure
    warning(paste("No distance specified, default of", df$Row_m[1]*500, "used")
    return(df)
  }
  else {
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
