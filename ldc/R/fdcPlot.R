#' Flow Duration Curve Plot
#'
#' This function will output a flow duration curve plot for a single monitoring station
#'
#'
#' @param flow.exceed, calculated flow exceedance values for individual station
#'
#' @param flow.est, dataframe: flow data for individual station -c('date', 'flow')
#'
#' @param ss.est, dataframe: stream stats data which includes FDPercent, FDest, lower, and upper
#'
#' @param plot.fn, file path for the flow duration curve plot to be saved in
#'
#' @param y.lims, initial FDC limits [c(1e-01, 1e+04)]
#'
#' @param x.px, width in pixels
#'
#' @param y.px, height in pixels
#'
#' @param pt.size, pointsize of plotted text
#' @
#' @return Flow Duration Curve Plot
#' @export
#'


fdc.ss.est.flow.plot <- function(flow.exceed=NULL,
                                 flow.est=NULL,
                                 ss.est=NULL,
                                 plot.fn=NULL,
                                 y.lims=NULL,
                                 x.px=900,
                                 y.px=750,
                                 pt.size=17) {


  options(warn=-1)
  ## send plot to pdf file
  if(!is.null(plot.fn)) {
    png(file=plot.fn,
        width=x.px,height=y.px, pointsize = pt.size,
        bg="white")
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
  if(names(dev.cur()) == "png") {
    dev.off()
  }
  options(warn=0)
}
