#'Flow duration curve estimate values
#'
#'This function will output a dataframe of flow duration estimates based on the Stream Stats XML file and ecoregion (???). However, this function is only set up for Ecoregion 1.
#'
#'
#'@param ss.fn, paste(st, stationID, .xml)
#'@param ss.path, path for streamstats xml (use getPath to find the correct directory, ie. getPath('StreamStatsBacteria'))
#'@param Eq.Region, default = 1
#'@return  st.ss.est dataframe of flow duration estimates
#'@export
#'
fdc.ss.estimate <- function(ss.fn=NULL,ss.path=NULL,
Eq.Region=1) {

  ### Document Me!!!!!!!!!!
  ### load library
  requireNamespace('XML')
  ### read StreamStats xml file
  ss.xml <- xmlParse(file=paste0(ss.path,ss.fn))
  ### get watershed parameters for USGS regression equations
  ### this is set up for region 1 only
  DA <- as.numeric(xpathSApply(ss.xml,"//*/PARAMETER[@name=\"DRNAREA\"]/@value"))
  P <- as.numeric(xpathSApply(ss.xml,"//*/PARAMETER[@name=\"PRECIP\"]/@value"))
  SC <- as.numeric(xpathSApply(ss.xml,"//*/PARAMETER[@name=\"WATCAP\"]/@value"))
  ### get regional parameters for USGS equations
  if(Eq.Region == 1) {
    Eq.Region <- data.frame(BCF=c(1.01508,1.01384,1.01555,1.02287,1.08456),
                            Exp10=c(-0.3834,-0.6488,-1.3592,-2.4906,-2.7120),
                            ExpDA=c(1.0067,1.0114,1.0116,1.0132,1.0559),
                            ExpP=c(0.8470,0.8927,1.0911,1.4513,1.6250),
                            ExpSC=c(0,0,0,0,1.3421),
                            SEE.T=c(0.078,0.074,0.079,0.097,0.190),
                            stringsAsFactors=FALSE)
  }
  else {
    print("Setup for Region 1 only")
    st.ss.est <- NULL
    return(st.ss.est)
  }
  ### create data.frame for results and calculate values
  st.ss.est <- data.frame(FDPercent = c(0.05,0.10,0.25,0.5,0.95),
                          FDEst=Eq.Region$BCF*(10^Eq.Region$Exp10)*(DA^Eq.Region$ExpDA)*(P^Eq.Region$ExpP)*(SC^Eq.Region$ExpSC),
                          stringsAsFactors=FALSE)
  st.ss.est <- data.frame(st.ss.est,
                          lower=(1/10^Eq.Region$SEE)*(st.ss.est$FDEst/Eq.Region$BCF),
                          upper=(10^Eq.Region$SEE)*(st.ss.est$FDEst/Eq.Region$BCF),
                          stringsAsFactors=FALSE)
  return(st.ss.est)
}
