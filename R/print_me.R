#' print_me
#'
#' This is a function for printing
#'
#' @export

print_me <- function(plot_name = NULL, filename = NULL, width = 1280, height = 853, pointsize = 20, res = 200, type = "cairo") {
  png(filename=paste(filename,".png", sep = ""),
      units="px",
      width=width,
      height=height,
      pointsize=pointsize,
      res=res,
      type=type)
  print(plot_name)
  dev.off()
}
