#' Licor Reader
#'
#' This function is used to extract sample data from the cumbersome
#' files generated from the licor respiration measuring device.
#' The file needs to be moved to the computer, opened with Excel,
#' and saved as a csv file before processing. When taking measurements, remarks should be entered that start
#' with the word "sample" i.e. "sample345".
#'
#' @export
licor_reader<-function(filepath) {

  df <- read.csv(filepath)
  colnames(df) <- df[8,]
  #dplyr::rename(df, Sample = HHMMSS)
  samples <- str_subset(df$HHMMSS, "sample\\s*(.*?)\\s*")
  samples2 <- gsub("[^0-9.-]", "", samples)
  samples3 <- substring(samples2, 7)
  df2 <- df[-(1:14), -24]
  samples4 <- df2 %>%
    rownames_to_column('rn') %>%
    filter(str_detect(HHMMSS, "sample"))%>%
    column_to_rownames('rn') %>% dplyr::select(c(-1, -(3:26))) %>% dplyr::rename(Sample = HHMMSS)
  samples4$Sample <- substring(gsub("[^0-9.-]", "", samples4$Sample), 7)
  samplerows <- as.numeric(rownames(samples4))
  datalist = list()
  for (i in 1:length(samplerows)){
    dat <- df[(samplerows[i]+5):(samplerows[i]+236),]
    datalist[[i]] <- dat
  }
  big_data = do.call(rbind, datalist)
  time.raw <- rep(seq(from = 0, to= 1155, by=5), length(samplerows))
  repeatedsamples <- rep(samples4$Sample, each=232)
  big_data$Sample <- repeatedsamples
  big_data$Time <- time.raw
  big_data$CO2R <- as.numeric(big_data$CO2R)
  big_data$CO2S <- as.numeric(big_data$CO2S)
  big_data <- big_data[,-24]
  return(big_data)
}
