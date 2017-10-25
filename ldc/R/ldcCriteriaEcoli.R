#'Load Duration Curve Criteria -- List of 2
#'
#'This function will create a list of two containing: 1) ldc.max (Date, flow, Vol.day, flow.exceed, load.day), 2) ldc.gmn (flow.exceed, Vol.day, load.day). This is an input for ldcPlot and scatterPlot
#'
#'@param flow.data, dataframe containing station, date (YYYY-MM-DD), flow_cfs
#'@return List of 2 containing $ldc.max and $ldc.gmn
#'@export


ldcCriteriaEcoli <- function(flow.data) {
  ## DOCUMENT ME!!!!!!!!!!!!!


  ## E.coli criteria
  max.crit <- 406
  gmn.crit <- 126
  ## Conversion factor from cfs to 100 ml/day
  ## 1 cfs x 28316.8 ml per cu ft * 1/100 * 24 hrs/day * 3600 sec/hrs
  vol.fac <- (28316.8/100)*(24*3600)
  ## Criteria data for ecoli station
  ##
  ## Calculate LDC for Maximum Criterion
  ldc.max  <- data.frame(Date=flow.data$date,
                         flow=flow.data$value,
                         Vol.day=vol.fac*flow.data$value,
                         flow.exceed = 100 - 100*rank(
                           flow.data$value, na.last="keep",
                           ties.method="average") /
                           length(flow.data$value),
                         load.day=vol.fac*flow.data$value*max.crit)
  ldc.max <- ldc.max[ order(ldc.max$flow.exceed), ]
  row.names(ldc.max) <- NULL
  ## Calculate LDC for Maximum Criterion
  ## Geometric mean of the loads within each flow zone are used for the
  ## LDC
  ldc.gmn <- data.frame(flow.exceed=c(10,40,60,90,100), Vol.day=NA, load.day=NA)
  ldc.gmn$Vol.day[1] <- 10^mean(log10(ldc.max[ ldc.max$flow.exceed >= 0
                                               & ldc.max$flow.exceed <= 10,
                                               ]$Vol.day
  )
  , na.rm = TRUE)

  ldc.gmn$load.day[1] <- ldc.gmn$Vol.day[1]*gmn.crit
  for(jj in 2:(length(ldc.gmn$flow.exceed)-1)) {
    ldc.vol <- ldc.max[ ldc.max$flow.exceed >
                          ldc.gmn$flow.exceed[jj-1] &
                          ldc.max$flow.exceed <=
                          ldc.gmn$flow.exceed[jj],
                        ]$Vol.day
    ldc.gmn$Vol.day[jj] <- 10^mean(log10(ldc.max[ ldc.max$flow.exceed >
                                                    ldc.gmn$flow.exceed[jj-1] &
                                                    ldc.max$flow.exceed <=
                                                    ldc.gmn$flow.exceed[jj],
                                                  ]$Vol.day
    )
    , na.rm = TRUE )
    ldc.gmn$load.day[jj] <- ldc.gmn$Vol.day[jj]*gmn.crit
  }
  ldc.gmn$Vol.day[5] <- 10^mean(log10(ldc.max[ ldc.max$flow.exceed > 90 &
                                                 ldc.max$flow.exceed <= 100,
                                               ]$Vol.day
  )
  , na.rm = TRUE)
  ldc.gmn$load.day[5] <- ldc.gmn$Vol.day[5]*gmn.crit
  ## Create list of the two data frames and return
  ldc.crit <- list(ldc.max=ldc.max,ldc.gmn=ldc.gmn)
  return(ldc.crit)
}
