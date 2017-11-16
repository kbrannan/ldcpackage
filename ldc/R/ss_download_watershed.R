ss.download.watershed <- function(workspaceID = NULL,
                                  ss.format = "SHAPE") {
  file.tmp <- tempfile()
  ss.url.download <- paste0("https://streamstatsags.cr.usgs.gov/streamstatsservices/download?workspaceID=",
                            workspaceID, "
                            &format=", ss.format)
  success <- download.file(ss.url.download, file.tmp, quiet = TRUE)
  if(success != 0) {
    return("Error downloading zip-file")
  } else {
    shp.data <- sapply(".", function(f) {
      fp <- file.path(temp, f)
      return(readOGR(".",shpfile))
      })
  }
}
