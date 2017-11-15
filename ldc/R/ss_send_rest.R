ss.send.rest <- function(ss.REST.query=NULL) {
  ss.responce <- try(RCurl::getURLContent(url = ss.REST.query),
                     silent = TRUE)
  if(class(ss.responce) == "try-error") {
    return("Error in REST querry string")
  } else {
    ss.json <- jsonlite::fromJSON(ss.responce)
    return(ss.json$workspaceID)
  }
}
