ss.send.rest <- function(ss.REST.query=NULL, connect.timeout = 180) {
  ss.opts <- RCurl::curlOptions(connecttimeout = connect.timeout)
  ss.responce <- try(RCurl::getURLContent(url = ss.REST.query,
                     silent = TRUE, .opts = ss.opts))
  if(class(ss.responce) == "try-error") {
    return("Error in REST querry string")
  } else {
    ss.json <- jsonlite::fromJSON(ss.responce)
    return(ss.json$workspaceID)
  }
}
