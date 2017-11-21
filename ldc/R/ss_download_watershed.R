ss.download.watershed <- function(workspaceID = NULL, chr.dir.download = NULL,
                                  ss.format = "SHAPE") {
  chr.file.zip <- paste0(chr.dir.download, "/ss_", workspaceID, ".zip")
  ss.url.download <- paste0("https://streamstatsags.cr.usgs.gov/streamstatsservices/download?workspaceID=",
                            workspaceID, "&format=", ss.format)
success <- download.file(ss.url.download, file.tmp, quiet = TRUE)

success <- download.file(ss.url.download, ch.file.zip, quiet = TRUE)

raw <- RCurl::getBinaryURL(ss.url.download)

con.file.zip <- file(chr.file.zip)
write(raw, con.file.zip)
close(con.file.zip)


  if(success != 0) {
    return("Error downloading zip-file")
  } else {
    shp.data <- sapply(".", function(f) {
      fp <- file.path(file.tmp, f)
      fp <- paste0(file.path(file.tmp), ".zip")
      junk <- rgdal::readOGR(fp)
      return(rgdal::readOGR(file.tmp))
      })
  }
}
