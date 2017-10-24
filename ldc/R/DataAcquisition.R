#'Query Bacteria Data from DEQ Databases
#'
#'This function will allow users to acquire necessary data from the DEQ databases to create Load Duration Curves
#'
#'
#'@param start_date, starting date of the form "'1990-01-01 00:00:00'"
#'@param end_date, ending date of the form "'1990-01-01 00:00:00'"
#'@param df_stns, dataframe of monitoring stations 'station', watershed 'watershed', and gage numbers 'gage'
#'@param chr_wtsd, the watershed of interest, ie. "Upper Yaquina River". Only need if saving into a directory ('save_direct'). Default is NULL.
#'@param save_direct, the directory to save files, default is NULL
#'@return bacteria.data dataframe of queried data
#'@export


bacteria.data <- function(start_date,
                          end_date,
                          df_stns,
                          chr_wtsd = NULL,
                          save_direct = NULL) {

  requireNamespace('RODBC')
  #for testing:
  #start_date <- "'1990-01-01 00:00:00'"
  #end_date <- paste0("'",Sys.Date()," 00:00:00'")
  #save_direct <-
  #df.stns <- df.stns

  ## get data directly from LASAR vis ODBC
  ##
  chr.wtsd <- chr_wtsd
  ## connect to LASAR and get data
  tmp.odbc <- RODBC::odbcConnect("DEQLEAD-LIMS") ##MR: changed LASAR access point from 'LASAR Web' 9/5/17
  ## get the names of all the tables in the database
  tmp.TableNames<- RODBC::sqlTables(tmp.odbc,errors = FALSE)
  ## get parameter information
  tmp.AllParameters <- RODBC::sqlFetch(tmp.odbc, "dbo.PARAMETER", stringsAsFactors=FALSE)
  sort(names(tmp.AllParameters))
  ## get all sample collecting organizations
  tmp.AllOrganizations <- RODBC::sqlFetch(tmp.odbc, "ORGANIZATION", stringsAsFactors=FALSE)
  ## get the indexes of parameter names that are simular to E. coli and
  ## fecal coliform
  tmp.Pars.index.coli <- grep("[cC]oli",tmp.AllParameters$NAME, value=FALSE)
  ## this is what we have
  tmp.AllParameters$NAME[tmp.Pars.index.coli]
  ## here are the keys for the coli-like parameters
  tmp.AllParameters$PARAMETER_KEY[tmp.Pars.index.coli]
  ## Uses all forms of "E. Coli" and "Fecal Coliform"
  tmp.Bacteria.Pars.index <- grep("(E. [Cc]oli|Fecal [Cc]oliform)",tmp.AllParameters$NAME)
  tmp.AllParameters$NAME[tmp.Bacteria.Pars.index]
  tmp.AllParameters$PARAMETER_KEY[tmp.Bacteria.Pars.index]
  ## get status key for A+ through F
  tmp.status.key <- RODBC::sqlFetch(tmp.odbc,"dbo.XLU_STATUS",stringsAsFactors=FALSE)
  tmp.status.key <- data.frame(unique(cbind(tmp.status.key$XLU_STATUS_KEY,tmp.status.key$STATUS,tmp.status.key$STATUS_CLASS)), stringsAsFactors=FALSE)
  names(tmp.status.key) <- c("XLU_STATUS_KEY","STATUS","STATUS_CLASS_KEY")
  tmp.status.key <- tmp.status.key[grep("^([A-F]${1}|A\\+)",tmp.status.key$STATUS),]
  tmp.status.key
  ## get status keys for status A+ to B
  tmp.status.keys <- tmp.status.key[grep("[^C-Z]",tmp.status.key$STATUS),]
  tmp.status.keys
  ## Set the Date-Time Range
  tmp.start.date <- start_date
  tmp.end.date <- end_date
  ## create query for LASAR
  tmp.query.data <- paste0("SELECT * FROM dbo.PARAMETER_RESULT LEFT JOIN (dbo.SAMPLE INNER JOIN dbo.STATION ON dbo.SAMPLE.STATION = dbo.STATION.STATION_KEY) ON dbo.PARAMETER_RESULT.SAMPLE = dbo.SAMPLE.SAMPLE_KEY WHERE dbo.SAMPLE.STATION IN (",paste0(df.stns$station,collapse=","), ") AND dbo.PARAMETER_RESULT.PARAMETER IN (", paste0(tmp.AllParameters$PARAMETER_KEY[tmp.Bacteria.Pars.index],collapse=","),") AND dbo.PARAMETER_RESULT.QA_QC_STATUS IN (",paste0(tmp.status.keys$XLU_STATUS_KEY,collapse=","),") AND dbo.SAMPLE.SAMPLE_DATE >= ",tmp.start.date, "AND dbo.SAMPLE.SAMPLE_DATE <= ",tmp.end.date,";")
  ## send query
  tmp.data <- RODBC::sqlQuery(tmp.odbc,tmp.query.data,stringsAsFactors = FALSE, na.strings = "NA")
  ## save query
      #tmp.q.file <- paste0(get.path("cleaned",wtsd=gsub(" ","",chr.wtsd)),"LASAR_query_",gsub(" ","",chr.wtsd),"_",end_date,".txt")
      #cat(paste0("SQL Query: ",tmp.query.data),file=tmp.q.file)
  ## exclude QC Field samples
  tmp.data <- tmp.data[tmp.data$QA_QC_TYPE %in% c(1,2),]
  ## add field for sampling organzation and populate
  tmp.data <- data.frame(tmp.data,ORGANIZATION="None",stringsAsFactors=FALSE)

  tmp.ORGANIZATION.key <- tmp.AllOrganizations[tmp.AllOrganizations$ORGANIZATION_KEY %in% unique(tmp.data$SAMPLING_ORGANIZATION),]
  for( ii in 1:length(tmp.ORGANIZATION.key$NAME)) {
    tmp.data$ORGANIZATION[tmp.data$SAMPLING_ORGANIZATION == tmp.ORGANIZATION.key$ORGANIZATION_KEY[ii]] <- tmp.ORGANIZATION.key$NAME[ii]
  }
  rm(ii)
  for(ii in 1:length(tmp.data$ORGANIZATION)) tmp.data$ORGANIZATION[ii] <- tmp.AllOrganizations$NAME[tmp.AllOrganizations$ORGANIZATION_KEY == tmp.data$SAMPLING_ORGANIZATION[ii]]

  ## add field for the letter value for the QAQC status
  tmp.data <- data.frame(tmp.data,QAQC="None", stringsAsFactors=FALSE)
  tmp.QAQC.key <- tmp.status.key[tmp.status.key$XLU_STATUS_KEY %in% unique(tmp.data$QA_QC_STATUS),]
  for( ii in 1:length(tmp.QAQC.key)) {
    tmp.data$QAQC[tmp.data$QA_QC_STATUS == tmp.QAQC.key$XLU_STATUS_KEY[ii]] <- tmp.QAQC.key$STATUS[ii]
  }
  rm(ii)
  ## check for non-numeric entries for bacteria concentrations
  tmp.nonnumeric.list <- unique(tmp.data[grep("[^0-9]",tmp.data$RESULT),]$RESULT)
  ## visually check the non-numeric entries
  tmp.nonnumeric.list
  ## this is an easy one, all the non-numeric parts can be removed by substitution and use the numeric part of string as RESULT
  tmp.data <- data.frame(tmp.data,RESULT.clean=tmp.data$RESULT)
  tmp.data$RESULT.clean <- gsub("[^0-9.]","",tmp.data$RESULT)
  ## do a quick check to see if all nonnumeric values were fixed
  unique(tmp.data[grep("[^0-9]",tmp.data$RESULT),]$RESULT.clean)
  ## looks good
  rm(tmp.nonnumeric.list)
  ## Translate Fecal Coliform to E. coli
  tmp.data <- data.frame(tmp.data,RESULT.trn=tmp.data$RESULT.clean,trns.yes.no="no", stringsAsFactors=FALSE)
  trn.fc.index <- grep("[Ff][Ee][Cc][Aa][Ll]",tmp.data$PARAMETER.NAME)

  #embed fc2ec function:
  fc2ec <- function(fc) {
    ## translates fecal coliform (fc) consentrations to E. coli (ec) concentrations
    ## using the equation developed by Cude (2005), which is:
    ##                   ec = 0.531 * fc ^ 1.06
    ## Reference: Cude, C.G., 2005. Accomodating Change of Bacteria Indicators
    ##            in Long Term Water Quality Datasets. Journal of American
    ##            Water Resources Association. pp. 47-54
    ec <- 0.531*fc^1.06
    return(ec)
  }

  if(length(trn.fc.index) > 0) {
    tmp.data$RESULT.trn[trn.fc.index] <- as.character(round(sapply(as.numeric(tmp.data$RESULT.clean[trn.fc.index]),fc2ec)))
    tmp.data$trns.yes.no[trn.fc.index] <- "yes"
  }
  ## clean up workspace
  rm(trn.fc.index)
  ## add a field to the bacteria adata data.frame for the sample date-time in a date-class format
  tmp.data <- data.frame(tmp.data,date=as.Date(strptime(gsub("(PDT|PST)","",tmp.data$SAMPLE_DATE), format="%Y-%m-%d")))
  ## check if all the orginal stations have data in the current subset
  sort(df.stns$station)
  sort(unique(tmp.data$STATION))
  ## based on TWG input, only data collected in 1993 and after will be used
  ## this is the data that weill be used to estimate LDCs
  bacteria.data <- subset(tmp.data, tmp.data$date >= as.Date("1993-01-01")) ##specific to MidCoast?
  ## use only stations in the bacteria.data
  df.stns <- df.stns[df.stns$station %in% unique(bacteria.data$STATION),]
  rm(tmp.data)
  ## save a copy of the data set
  #save(bacteria.data,file=paste0(get.path(folder="cleaned",wtsd=gsub(" ","",chr.wtsd)),"bacteria_DataCleaning_",gsub(" ","",chr.wtsd),"_",chr.stamp,".RData"))
  return (bacteria.data)
}

# bact <- bacteria.data(start_date = "'1990-01-01 00:00:00'",
#                       end_date = paste0("'",Sys.Date()," 00:00:00'"),
#                       df.stns = df.stns,
#                       save_direct = NULL)
