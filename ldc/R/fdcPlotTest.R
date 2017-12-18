## fdcPlot TEST ##

## Load functions
source("//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Bacteria/LDC/Calculations/Rscripts/LDC Functions.R")


## Run Flow Exceed function to calculate flow exceed values
## I renamed flow.exceed to be tmp.exceed


 tmp.exceed <- function(v.flow) {
  tmp.rank <- rank(v.flow, ties.method = "average")
  tmp.exceed <- tmp.rank / length(v.flow)
  tmp.exceed <- 100 * (1 - tmp.exceed)
  return(tmp.exceed)
}

 v.flow <- df.flow$flow

 tmp.exceed(v.flow)


## ss.est = stream stats estimate - call funtion for this

## Define station to get stream stats from, make sure this xml file is in the folder at the end of the file path below
tmp.ss.est.fn <- paste0("st",11476,".xml")

fdc.ss.estimate(ss.fn=tmp.ss.est.fn ,ss.path="//deqhq1/tmdl/TMDL_WR/MidCoast/Data/Bacteria/StreamStatsData/BacteriaStations/",
                             Eq.Region=1)

tmp.fdc.ss.estimate <- fdc.ss.estimate



## path to write the figure
path.fdc.plot.figure <- "//deqhq1/TMDL/TMDL_WR/MidCoast/Models/Bacteria/LDC/Bernadette-workspace/figures"

## name of output figure file
tmp.plot.fn <- "fdcPlot-test.png"

## file for function
fdc.plot.figure <- paste0(path.fdc.plot.figure, "/", tmp.plot.fn)


## define y.lims
y.lim.fdc <- c(1e+15,1E-05)



fdc.ss.est.flow.plot(flow.exceed=tmp.exceed(v.flow),
                     flow.est=df.flow$flow,
                     ss.est=fdc.ss.estimate(ss.fn=tmp.ss.est.fn ,ss.path="//deqhq1/tmdl/TMDL_WR/MidCoast/Data/Bacteria/StreamStatsData/BacteriaStations/",
                                            Eq.Region=1),
                     plot.fn=fdc.plot.figure,
                     y.lims=y.lim.fdc,
                     x.px=900,
                     y.px=750,
                     pt.size=17)
dev.off()

  