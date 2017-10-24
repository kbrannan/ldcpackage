#'Scatter Bacteria Time Series Plot
#'
#'This function will create a scatter plot of bacteria data over time
#'
#'@param ldc, list of 4 containing $points (Date, ecoli, digression, reduction, flow, Vol.day, flow.exceed, flow.zone, and load.day);
#'$summary.points (N, Digressions, Max.Reduction, Max.Reduction.FLZ, Max.Reduction.flow.exceed, Max.Reduction.ecoli);
#'$geomns (flow.exceed, Vol.day, load.day, conc, N, digression, reduction, desc);
#'$summary.geomns (N, Digressions, Max.Reducition, Max.Reduction.FLZ, Max.Reduction.conc)
#'@param ldc.crit, list of 2 containing $ldc.max (Date, flow, vol.day, flow.exceed, load.day);
#'$ldc.gmn (flow.exceed, Vol.day, load.day)
#'@param est.flow, dataframe including date, value, flow.exceed
#'@param plot.fn, filepath for the scatterplot to be saved in. Default is NULL.
#'@return bacteria time series scatter plot
#'@export
#'

scatterBacteriaTSPlot <- function(ldc,
                                          ldc.crit,
                                          est.flow,
                                          plot.fn=NULL) {


  #change if necessary
  x.lim.scatter <- c("1991-01-01", "2014-12-31")
  y.lim.flow <- c('1e-02', '1e+03')
  y.lim.scatter.bacteria <- c(1, 10000)

   ## check for correct input
  if(is.null(ldc) | is.null(ldc.crit) | is.null(est.flow)) {
    print("Check input! Functions requires ldc. ldc.crit, and est.flow")
    return()
  }

  ## set output for plot
  ## if no filename, output sent to current device
  if(is.null(plot.fn ) != TRUE) {
    pdf(file=plot.fn,
        width=11,heigh=8.5,onefile=FALSE,title="",
        paper="special", bg="white")
  }
  ## set plot margins to accomodate the secondary
  ## y-axis
  par(mar=c(5, 4, 4, 4) + 0.1)
  ## get x limts from dates of ldc and flow-ts
  if( is.null(x.lim.scatter)) {
    x.lim.scatter <- as.POSIXlt(
      c(min(c(range(ldc.crit$ldc.max$Date),
              range(ldc$points$Date),
              range(est.flow$date))),
        max(c(range(ldc.crit$ldc.max$Date),
              range(ldc$points$Date),
              range(est.flow$date)))
      )
    )
    x.lim.scatter[1]$year <- x.lim.scatter[1]$year - 2
    x.lim.scatter[2]$year <- x.lim.scatter[2]$year + 1
    x.lim.scatter <- as.Date(x.lim.scatter)
  }
  ## get limits for the bacteria axis
  if(is.null(y.lim.scatter.bacteria)) {
    y.lim.scatter.bacteria <- c(100,10)
    if(y.lim.scatter.bacteria[1] > 10^floor(log10(min(ldc$points$ecoli,na.rm=TRUE)))) {
      y.lim.scatter.bacteria[1] = 10^floor(log10(min(ldc$points$ecoli,na.rm=TRUE)))
    }
    if(y.lim.scatter.bacteria[2] < 10^ceiling(log10(max(ldc$points$ecoli,na.rm=TRUE)))) {
      y.lim.scatter.bacteria[2] = 10^ceiling(log10(max(ldc$points$ecoli,na.rm=TRUE)))
    }
  }
  ## create temporary data.frame for flow
  tmp.flow <- data.frame(Date=ldc.crit$ldc.max$Date,
                         flow=ldc.crit$ldc.max$flow,
                         stringsAsFactors=FALSE)
  tmp.flow <- tmp.flow[order(tmp.flow$Date),]
  if(is.null(y.lim.flow)) {
    ## get limits for flow axis
    y.lim.flow <- c(100,10)
    if(y.lim.flow[1] > 10^floor(log10(min(tmp.flow$flow,na.rm=TRUE)))) {
      y.lim.flow[1] = 10^floor(log10(min(tmp.flow$flow,na.rm=TRUE)))
    }
    if(y.lim.flow[2] < 10^ceiling(log10(max(tmp.flow$flow,na.rm=TRUE)))) {
      y.lim.flow[2] = 10^ceiling(log10(max(tmp.flow$flow,na.rm=TRUE)))
    }
  }
  ## set up plot plot using bacteria observations
  plot(ldc$points$Date,ldc$points$ecoli,
       xlim=x.lim.scatter,ylim=y.lim.scatter.bacteria,
       ylab="E.coli (org/100 ml)", log="y",xlab="Date",cex=0)
  ## plot flow on secondary y-axis
  par(new=T)
  mtext("Stream Flow (cfs)",side=4,line=3,col="blue")
  axis(4,lines(tmp.flow$Date,tmp.flow$flow,
               xlim=x.lim.scatter,ylim=y.lim.flow,
               col="blue"),
       col.axis="blue",col.lab="blue", col="blue"
  )
  ## add flow zone lines
  plt.col = "blue"
  plt.ldw = 2.5
  plt.lty="dotted"
  flow.zones.flow <- c()
  flow.zones <- c(10,40,60,90)
  for(ii in 1:4) {
    tmp.flow.zone.flow <- subset(est.flow, round(flow.exceed,0) == flow.zones[ii])
    flow.zone.index <- which.min(abs(subset(tmp.flow.zone.flow, round(flow.exceed,0) == flow.zones[ii])$flow.exceed-flow.zones[ii]))
    flow.zones.flow <- c(flow.zones.flow,tmp.flow.zone.flow$value[flow.zone.index])
  }
  axis(4,abline(h=flow.zones.flow, col=plt.col,
                lwd=plt.ldw, lty=plt.lty, ),
       col.axis="blue",col.lab="blue", col="blue")
  ## add text for flow zones descriptions
  fz.lbls <- ldc$geomns$desc
  text( x.lim.scatter[2],
        10^mean(log10(c(par("yaxp")[2]/10,par("yaxp")[2]))),
        fz.lbls[1], adj=c(0.5,0.5),
        srt=90, col="blue", cex=0.8)
  text( x.lim.scatter[2],
        10^mean(log10(flow.zones.flow[1:2])),
        gsub(" "," \n", fz.lbls[2]), adj=c(0.5,0.5),
        srt=90, col="blue", cex=0.8)
  text( x.lim.scatter[2],
        10^mean(log10(flow.zones.flow[2:3])),
        gsub(" "," \n", fz.lbls[3]), adj=c(0.5,0.5),
        srt=90, col="blue", cex=0.8)
  text( x.lim.scatter[2],
        10^mean(log10(flow.zones.flow[3:4])),
        fz.lbls[4], adj=c(0.5,0.5),
        srt=90, col="blue", cex=0.8)
  text( x.lim.scatter[2],
        10^mean(log10(c(flow.zones.flow[4],par("yaxp")[1]))),
        fz.lbls[5], adj=c(0.5,0.5),
        srt=90, col="blue", cex=0.8)
  ## plot bacteria on primary y-axis
  par(new=T)
  ## plot digressions in red and bigger point
  ldc.yes.D <- subset(ldc$points,ldc$points$digression == 1)
  plot(ldc.yes.D$Date,ldc.yes.D$ecoli,
       xlim=x.lim.scatter,ylim=y.lim.scatter.bacteria,
       ylab="", log="y",xlab="",type="p",
       axes=F,pch=21,bg="red",cex=1.5)
  ## plot no digression in green and smaller point
  ldc.no.D <- subset(ldc$points,ldc$points$digression == 0)
  points(ldc.no.D$Date,ldc.no.D$ecoli,
         xlim=x.lim.scatter,ylim=y.lim.scatter.bacteria,
         pch=21,bg="green",cex=0.9)
  ## add criteria lines and text
  x.text <- par("xaxp")[1] - (par("xaxp")[2]-par("xaxp")[1])/(1.2*par("xaxp")[3])
  text(x.text,500,"Max\n406 org/100ml",
       cex=0.7, col="red",adj=c(0,0))
  abline(h=406, col="red",lwd=2, lty="dashed")
  text(x.text,150,"GeoMean\n126 org/100ml",
       cex=0.7,col="red",adj=c(0,0))
  abline(h=126, col="red",lwd=2, lty="dashed")
  ## if filename, close pdf device
  if(is.null(plot.fn ) != TRUE) {
    dev.off()
  }
}
