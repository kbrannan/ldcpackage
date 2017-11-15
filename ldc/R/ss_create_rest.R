ss.create.rest <- function(x.loc = NULL,
                           y.loc = NULL,
                           ss.url ="https://streamstatsags.cr.usgs.gov/streamstatsservices",
                           ss.prefix ="watershed",
                           response.format = "geojson",
                           rcode = "OR",
                           crs=NULL,
                           includeparameters = "false",
                           includeflowtypes = "false",
                           includefeatures = "true",
                           simplify = "true") {
  ss.REST.query <- paste0(ss.url, "/", ss.prefix, ".", response.format,
                     "?rcode=", rcode,
                     "&xlocation=", x.loc,
                     "&ylocation=", y.loc,
                     "&crs=", crs,
                     "&includeparameters=", includeparameters,
                     "&includeflowtypes=", includeflowtypes,
                     "&includefeatures=", includefeatures,
                     "&simplify=", simplify
                     )

  return(ss.REST.query)
}

