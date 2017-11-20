## fdcPlot TEST ##

## Load functions
source("//deqhq1/tmdl/TMDL_WR/MidCoast/Models/Bacteria/LDC/Calculations/Rscripts/LDC Functions.R")

## flow.exceed = tmp.flow.est - Function

flow.exceed <- function(v.flow) {
  tmp.rank <- rank(v.flow, ties.method = "average")
  tmp.exceed <- tmp.rank / length(v.flow)
  tmp.exceed <- 100 * (1 - tmp.exceed)
  return(tmp.exceed)
}

## Use flow exceed function to calculate flow exceed
tmp.exceed <- flow.exceed

## flow.est = flow values
tmp.fdc.ss.estimate <- fdc.ss.estimate

##ss.est = stream stats estimate - call funtion for this


## path to write the figure
fdc.plot.figure <- "//deqhq1/TMDL/TMDL_WR/Models/Bacteria/LDC/Bernadette-workspace/Figures"

## name of output figure file
tmp.plot.fn <- "fdcPlot-test.png"

## define y.lims
y.lim.fdc <- c(1e+15,1E-05)



fdc.ss.est.flow.plot(flow.exceed=tmp.exceed,
                     flow.est=df.flow$flow,
                     ss.est=tmp.fdc.ss.estimate,
                     plot.fn=fdc.plot.figure,
                     y.lims=y.lim.fdc) 
  
  
  options(warn=-1)
  ## send plot to pdf file
  if(!is.null(plot.fn)) {
    pdf(file=plot.fn,
        width=11,heigh=8.5,onefile=FALSE,title="",
        paper="special", bg="white")
  }
  
  ## Reorganize flow data into dataframe and then sort data on flow exceedance
  tmp.plot <- data.frame(x=flow.exceed,y=flow.est,
                         stringsAsFactors=FALSE)
  tmp.plot <- tmp.plot[order(tmp.plot$x),]
  
  ## calculate the y-axis limit if they are not given
  if(is.null(y.lims) == TRUE) {
    y.lims <- c(10^floor(log10(min(tmp.plot$y))),
                10^ceiling(log10(max(tmp.plot$y))))
  }
  ## plot flow duration curve
  plot(tmp.plot$x,tmp.plot$y,
       log="y",
       xlab="Flow Exceedance", ylab="Flow (cfs)",
       ylim=y.lims,
       xlim=c(-0.3,100.3), xaxp=c(0,100,10), xaxs="i",
       col="black",lty="solid",lwd=1.75,type="l"
  )
  ## add flow zone lines
  plt.col = "gray"
  plt.ldw = 2.5
  plt.lty="solid"
  abline(v=c(10,40,60,90), col=plt.col, lwd=plt.ldw, lty=plt.lty)
  ## text for descriptions of flow zones
  text( 5,y.lims[1]*1.0,"High Flows", adj=c(0.5,0))
  text(25,y.lims[1]*1.0,"Transitional Flows", adj=c(0.5,0))
  text(50,y.lims[1]*1.0,"Typical Flows", adj=c(0.5,0))
  text(75,y.lims[1]*1.0,"Dry Flows", adj=c(0.5,0))
  text(95,y.lims[1]*1.0,"Low Flows", adj=c(0.5,0))
  ## plot Stream Stats Estmates with error bars
  ## plot estimates
  points(ss.est$FDPercent*100,ss.est$FDEst,
         pch=19,cex=1.1)
  errbar <- 0.75 ## error bar width
  ## plotting error bars
  for(ii in 1:length(ss.est$FDEst)) {
    lines(c(ss.est$FDPercent[ii],ss.est$FDPercent[ii])*100,
          c(ss.est$lower[ii],ss.est$upper[ii]))
    lines(c(ss.est$FDPercent[ii]*100-errbar,ss.est$FDPercent[ii]*100+errbar),
          c(ss.est$lower[ii],ss.est$lower[ii]))
    lines(c(ss.est$FDPercent[ii]*100-errbar,ss.est$FDPercent[ii]*100+errbar),
          c(ss.est$upper[ii],ss.est$upper[ii]))
  }
  ## close device if plot sent to pdf file
  if(names(dev.cur()) == "pdf") {
    dev.off()
  }
  options(warn=0)
}

  
  
 

png(filename= paste0(fdc.plot.figure,"/",tmp.plot.fn)
    