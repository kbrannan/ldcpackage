#'LDC CALC
#'
#'This function will output a list containing:
#'points: dataframe which includes the date, ecoli result, digression and reduction value, flow, vol per day, flow exceedance, flow zone (transitional, dry, etc), and load/day
#'summary.points: dataframe summary of points dataframe which includes, N, digressions, max reductions, max reuction flows, max reduction flow exceed, and max reduction ecoli
#'geomns: dataframe which includes flow exceedance, vol/day, concentration, N, digression and reduction, and flow zone description
#'summary.geomns: summary of geomns dataframe, including N, Digressions, Max reduction, Max reduction flows, and max reduction concentration
#'
#'@param df.stn.ecoli, dataframe: bacteria data for indivdual station-- c('Date', 'value')
#'@param df.stn.flow, dataframe: flow data for individual station-- c('date', 'value', 'flow.exceed')
#'@return list containing four dataframes: points, summary.points, gemomns, and summary.geomns
#'@export
#'

ldcCalc <- function(df.stn.ecoli,
                    df.stn.flow) {
  requireNamespace('fdcCalc')
  ## criteria concentrations
  gmn.crit <- 126
  max.crit <- 406

  df.stn.flow <- cbind(df.stn.flow,
                       flow.exceed = flow.exceed(df.stn.flow$value))

  ## gets flows for the days samples were collected
  flow.ecoli <- merge(data.frame(date.chr = strftime(df.stn.ecoli$Date, "%Y-%m-%d"), ##MR: changed to capital 'D' in 'date' on 9/5/2017
                                 conc = df.stn.ecoli$value),
                      data.frame(date.chr = strftime(df.stn.flow$date, "%Y-%m-%d"),
                                 flow = df.stn.flow$value,
                                 flow.exceed = df.stn.flow$flow.exceed))

  ##flow.ecoli <- get.flow(df.stn.ecoli$Date, df.stn.flow)
  ## Conversion factor from cfs to 100 ml/day
  ## 1 cfs x 28316.8 ml per cu ft * 1/100 * 24 hrs/day * 3600 sec/hrs
  vol.fac <- (28316.8/100)*(24*3600)
  ## Calculate points
  ldc.points <- data.frame(Date=df.stn.ecoli$Date, ##MR: changed to capital 'D' in 'date' on 9/5/2017
                           ecoli=df.stn.ecoli$value,
                           digression=as.numeric(df.stn.ecoli$value >= max.crit),
                           reduction=as.numeric(df.stn.ecoli$value >= max.crit)*
                             ceiling(100*(max.crit-df.stn.ecoli$value)/df.stn.ecoli$value),
                           flow=flow.ecoli$flow,
                           Vol.day=vol.fac*flow.ecoli$flow,
                           flow.exceed = flow.ecoli$flow.exceed,
                           flow.zone= sapply(flow.ecoli$flow.exceed,get.flow.zone),
                           load.day=vol.fac*flow.ecoli$flow*df.stn.ecoli$value
  )
  ## Create summary data frame for max criterion
  summary.max <- data.frame(N=NA,Digressions=NA,Max.Reduction=NA,Max.Reduction.FLZ=NA,
                            Max.Reduction.flow.exceed=NA, Max.Reduction.ecoli=NA)
  ## Populate the summary data frame for max criterion
  summary.max$N <- length(ldc.points$ecoli)
  summary.max$Digressions <- sum(ldc.points$digression)
  reductions.max <- min(ldc.points$reduction)
  if(reductions.max < 0) {
    Max.Reduction.row <- grep(as.character(min(ldc.points$reduction)),
                              as.character(ldc.points$reduction))
    if(length(Max.Reduction.row) > 1) {
      summary.max$Max.Reduction <- ldc.points$reduction[Max.Reduction.row[1]]
      summary.max$Max.Reduction.FLZ <- as.character(ldc.points$flow.zone[Max.Reduction.row[1]])
      summary.max$Max.Reduction.flow.exceed <- ldc.points$flow.exceed[Max.Reduction.row[1]]
      summary.max$Max.Reduction.ecoli <- ldc.points$ecoli[Max.Reduction.row[1]]
      for(jj in 2:length(Max.Reduction.row)) {
        summary.max <- rbind(summary.max,
                             data.frame(N=length(ldc.points$ecoli),
                                        Digressions=sum(ldc.points$digression),
                                        Max.Reduction=ldc.points$reduction[Max.Reduction.row[jj]],
                                        Max.Reduction.FLZ=as.character(ldc.points$flow.zone[Max.Reduction.row[jj]]),
                                        Max.Reduction.flow.exceed=ldc.points$flow.exceed[Max.Reduction.row[jj]],
                                        Max.Reduction.ecoli=ldc.points$ecoli[Max.Reduction.row[jj]]
                             )
        )
      }
    }
    else {
      summary.max$Max.Reduction <- ldc.points$reduction[Max.Reduction.row]
      summary.max$Max.Reduction.FLZ <- as.character(ldc.points$flow.zone[Max.Reduction.row])
      summary.max$Max.Reduction.flow.exceed <- ldc.points$flow.exceed[Max.Reduction.row]
      summary.max$Max.Reduction.ecoli <- ldc.points$ecoli[Max.Reduction.row]
    }
  }
  if(length(ls(pattern = "Max.Reduction.row")) > 0) rm(Max.Reduction.row)
  ## Calculate geomeans
  ## used "flow.zone" from points and "desc" from geomns to
  ## seperate the concentrations for flow ranges
  flow.geomns <-  data.frame(flow.exceed = flow.exceed(df.stn.flow$value),
                             Vol.day = vol.fac * df.stn.flow$value)
  ldc.geomns <- data.frame(flow.exceed=c(10,40,60,90,100),
                           Vol.day=NA,
                           load.day=NA,
                           conc=NA,
                           N=0,
                           digression=0,
                           reduction=0,
                           desc=c("High Flows","Transitional Flows",
                                  "Typical Flows","Dry Flows","Low Flows"),
                           stringsAsFactors=FALSE)

  ldc.geomns$N <- do.call(rbind,lapply(1:length(ldc.geomns$desc),
                                       function(x,y,z) length(y[y$flow.zone == z$desc[x], 1]),
                                       ldc.points, ldc.geomns))
  ldc.geomns$conc <- do.call(rbind,lapply(1:length(ldc.geomns$desc),
                                          function(x,y,z) 10^mean(log10(y[y$flow.zone == z$desc[x], "ecoli"])),
                                          ldc.points, ldc.geomns))

  if(length(ldc.points[ ldc.points$flow.exceed >= 0 &
                        ldc.points$flow.exceed <= 10,
                        ]$load.day) >= 1) {
    ldc.geomns$N[1] <- length(ldc.points[ ldc.points$flow.exceed >= 0 &
                                            ldc.points$flow.exceed <= 10,
                                          ]$load.day)
    ## Only caclulate geomean if there is more than one observation within the flow zone
    if(ldc.geomns$N[1] > 1) {
      # ldc.geomns$load.day[1] <- 10^mean(log10(na.omit(ldc.points[
      #                                                            ldc.points$flow.exceed >= 0 &
      #                                                            ldc.points$flow.exceed <= 10,
      #                                                           ]$load.day
      #                                                 )
      #                                         )
      #                                   )
      # ldc.geomns$Vol.day[1] <- 10^mean(log10(na.omit(ldc.points[
      #                                                           ldc.points$flow.exceed >= 0 &
      #                                                           ldc.points$flow.exceed <= 10,
      #                                                          ]$Vol.day
      #                                                )
      #                                        )
      #                                 )
      ldc.geomns$conc[1] <- 10^mean(log10(na.omit(ldc.points[
        ldc.points$flow.exceed >= 0 & ldc.points$flow.exceed <= 10, ]$ecoli)))
      ldc.geomns$Vol.day[1] <- 10^mean(log10(na.omit(flow.geomns[
        flow.geomns$flow.exceed >= 0 & flow.geomns$flow.exceed <= 10, ]$Vol.day)))
    }
  }

  for(jj in 2:(length(ldc.geomns$flow.exceed)-1)) {
    if(length(na.omit(ldc.points[ ldc.points$flow.exceed >  ldc.geomns$flow.exceed[jj-1] &
                                  ldc.points$flow.exceed <= ldc.geomns$flow.exceed[jj],
                                  ]$load.day
    )
    ) >= 1
    ) {
      ldc.geomns$N[jj] <- length(na.omit(ldc.points[ ldc.points$flow.exceed >  ldc.geomns$flow.exceed[jj-1] &
                                                       ldc.points$flow.exceed <= ldc.geomns$flow.exceed[jj],
                                                     ]$load.day
      )
      )
      ## Only caclulate geomean if there is more than one observation within the flow zone
      if(ldc.geomns$N[jj] > 1) {
        # ldc.geomns$load.day[jj] <-
        #   10^mean(log10(na.omit(ldc.points[ ldc.points$flow.exceed >  ldc.geomns$flow.exceed[jj-1] &
        #                                     ldc.points$flow.exceed <= ldc.geomns$flow.exceed[jj],
        #                                   ]$load.day
        #                         )
        #                 )
        #           )
        # ldc.geomns$Vol.day[jj] <-
        #   10^mean(log10(na.omit(ldc.points[ ldc.points$flow.exceed >  ldc.geomns$flow.exceed[jj-1] &
        #                                     ldc.points$flow.exceed <= ldc.geomns$flow.exceed[jj],
        #                                   ]$Vol.day
        #                         )
        #                 )
        #           )
        ldc.geomns$Vol.day[jj] <- 10^mean(log10(na.omit(flow.geomns[
          flow.geomns$flow.exceed >  ldc.geomns$flow.exceed[jj-1] &
            flow.geomns$flow.exceed <= ldc.geomns$flow.exceed[jj], ]$Vol.day)))
        ldc.geomns$conc[jj] <- 10^mean(log10(na.omit(ldc.points[
          ldc.points$flow.exceed > ldc.geomns$flow.exceed[jj-1] &
            ldc.points$flow.exceed <= ldc.geomns$flow.exceed[jj], ]$ecoli)))
      }
    }
  }
  if(length(na.omit(ldc.points[ ldc.points$flow.exceed > 90 &
                                ldc.points$flow.exceed <= 100,
                                ]$load.day
  )
  ) > 1
  ) {
    ldc.geomns$N[5] <- length(na.omit(ldc.points[ ldc.points$flow.exceed > 90 &
                                                    ldc.points$flow.exceed <= 100,
                                                  ]$load.day
    )
    )
    ## Only caclulate geomean if there is more than one observation within the flow zone
    if(ldc.geomns$N[5] > 1) {
      # ldc.geomns$load.day[5] <- 10^mean(log10(na.omit(ldc.points[ldc.points$flow.exceed > 90 &
      #                                                            ldc.points$flow.exceed <= 100,
      #                                                           ]$load.day
      #                                                 )
      #                                         )
      #                                   )
      # ldc.geomns$Vol.day[5] <- 10^mean(log10(na.omit(ldc.points[ldc.points$flow.exceed > 90 &
      #                                                           ldc.points$flow.exceed <= 100,
      #                                                          ]$Vol.day
      #                                                )
      #                                        )
      #                                  )
      ldc.geomns$conc[5] <- 10^mean(log10(na.omit(ldc.points[
        ldc.points$flow.exceed > 90 & ldc.points$flow.exceed <= 100, ]$ecoli)))
      ldc.geomns$Vol.day[5] <- 10^mean(log10(na.omit(flow.geomns[
        flow.geomns$flow.exceed > 90 & flow.geomns$flow.exceed <= 100, ]$Vol.day)))

    }
  }
  ldc.geomns$load.day <- ldc.geomns$conc * ldc.geomns$Vol.day

  ldc.geomns[!is.na(ldc.geomns$conc), ]$digression <- as.numeric(ldc.geomns[!is.na(ldc.geomns$conc), ]$conc
                                                                 >= gmn.crit)
  ldc.geomns$reduction <- ldc.geomns$digression * 100 * (gmn.crit - ldc.geomns$conc) / ldc.geomns$conc
  ## Create summary data frame for max criterion
  summary.gmn <- data.frame(N=NA,Digressions=0,Max.Reduction=0,Max.Reduction.FLZ=NA,
                            Max.Reduction.conc=NA)
  ## Populate the summary data frame for geomean criterion
  summary.gmn$N <- sum(as.numeric(!is.na(ldc.geomns$conc)))
  summary.gmn$Digressions <- sum(ldc.geomns$digression, na.rm=TRUE)
  summary.gmn$Max.Reduction <- min(ldc.geomns$reduction, na.rm=TRUE)
  if(summary.gmn$Max.Reduction < 0) {
    Max.Reduction.row <- min(grep(as.character(summary.gmn$Max.Reduction),as.character(na.omit(ldc.geomns$reduction))))

    summary.gmn$Max.Reduction.FLZ <- as.character(na.omit(ldc.geomns)$desc[Max.Reduction.row])
    summary.gmn$Max.Reduction.conc <- na.omit(ldc.geomns)$conc[Max.Reduction.row]
    rm(Max.Reduction.row)
  }
  ## Create LDC for station
  ldc.ecoli <- list(points=ldc.points, summary.points=summary.max,
                    geomns=ldc.geomns, summary.geomns=summary.gmn)
  ## Return LDC
  return(ldc.ecoli)

}
