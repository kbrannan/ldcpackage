#'Load Duration Curve Plot
#'
#'This function will output a load duration curve plot for a single monitoring station
#'
#'
#'@param ldc.stn, list of 4 containing $points (Date, ecoli, digression, reduction, flow, Vol.day, flow.exceed, flow.zone, and load.day);
#'$summary.points (N, Digressions, Max.Reduction, Max.Reduction.FLZ, Max.Reduction.flow.exceed, Max.Reduction.ecoli);
#'$geomns (flow.exceed, Vol.day, load.day, conc, N, digression, reduction, desc);
#'$summary.geomns (N, Digressions, Max.Reducition, Max.Reduction.FLZ, Max.Reduction.conc)
#'@param ldc.crit, list of 2 containing $ldc.max (Date, flow, vol.day, flow.exceed, load.day);
#'$ldc.gmn (flow.exceed, Vol.day, load.day)
#'@param y.lims, initial LDC limits [c(1e+15,1E-05)]
#'@
#'@return  Load Duration Curve plot
#'@export
#'

ldc.plot <- function(ldc.stn,
                     ldc.crit,
                    # plot.fn=NULL,
                     y.lims=NULL) {

  options(warn=-1)

  # Uncomment to add file path for plot to be saved in
  # if(!is.null(plot.fn)) {
  #   pdf(file=plot.fn,
  #       width=11,heigh=8.5,onefile=FALSE,title="",
  #       paper="special", bg="white")
  # }

  if(is.null(y.lims)) {
    min.y <- min(
      c(min(ldc.stn$points$load.day, na.rm=TRUE),
        min(ldc.crit$ldc.max$load.day, na.rm=TRUE)
      )
    ) / 2.5
    max.y <- max(
      c(max(ldc.stn$points$load.day, na.rm=TRUE),
        max(ldc.crit$ldc.max$load.day, na.rm=TRUE)
      )
    ) * 1
    y.lims <- c(min.y,max.y)
  }
  ## Set line plotting variables
  crt.col="red"
  crt.lty="dashed"
  crt.lwd=2.7
  ## set up the plot
  #   plot(ldc.crit$max$flow.exceed,ldc.crit$max$load.day, log="y",
  #        xlab="Flow Exceedance", ylab="Load / day",
  #        ylim=y.lims,xlim=c(0,100), xaxp=c(0,100,10),
  #        col=crt.col, lty=crt.lty, lwd=crt.lwd, xaxs="i"
  #       )
  plot(ldc.crit$max$flow.exceed,ldc.crit$max$load.day, log="y",
       xlab="Flow Exceedance", ylab="Load / day",
       ylim=y.lims,xlim=c(-0.3,100.3), xaxp=c(0,100,10),
       col=crt.col, lty=crt.lty, lwd=crt.lwd, xaxs="i"
  )
  ## add flow zone lines
  plt.col = "gray"
  plt.ldw = 2.5
  plt.lty="solid"
  abline(v=c(10,40,60,90), col=plt.col, lwd=plt.ldw, lty=plt.lty)
  ## text for descriptions of flow zones
  fz.lbls <- ldc.stn$geomns$desc
  text( 5,y.lims[1]*1.0,fz.lbls[1], adj=c(0.5,0))
  text(25,y.lims[1]*1.0,fz.lbls[2], adj=c(0.5,0))
  text(50,y.lims[1]*1.0,fz.lbls[3], adj=c(0.5,0))
  text(75,y.lims[1]*1.0,fz.lbls[4], adj=c(0.5,0))
  text(95,y.lims[1]*1.0,fz.lbls[5], adj=c(0.5,0))
  ## plot crit 406
  lines(ldc.crit$ldc.max$flow.exceed,ldc.crit$ldc.max$load.day,
        col=crt.col, lty=crt.lty, lwd=crt.lwd
  )
  ## plot the crit 126 geomeans
  lines(c(0,ldc.crit$ldc.gmn$flow.exceed[1]),
        c(ldc.crit$ldc.gmn$load.day[1],ldc.crit$ldc.gmn$load.day[1]),
        col=crt.col, lty=crt.lty, lwd=crt.lwd
  )
  lines(c(ldc.crit$ldc.gmn$flow.exceed[1],ldc.crit$ldc.gmn$flow.exceed[1]),
        c(ldc.crit$ldc.gmn$load.day[1],ldc.crit$ldc.gmn$load.day[2]),
        col=crt.col, lty=crt.lty, lwd=crt.lwd
  )
  for(ii in 2:(length(ldc.crit$ldc.gmn$flow.exceed)-1)) {
    lines(c(ldc.crit$ldc.gmn$flow.exceed[ii-1],ldc.crit$ldc.gmn$flow.exceed[ii]),
          c(ldc.crit$ldc.gmn$load.day[ii],ldc.crit$ldc.gmn$load.day[ii]),
          col=crt.col, lty=crt.lty, lwd=crt.lwd
    )
    lines(c(ldc.crit$ldc.gmn$flow.exceed[ii],ldc.crit$ldc.gmn$flow.exceed[ii]),
          c(ldc.crit$ldc.gmn$load.day[ii],ldc.crit$ldc.gmn$load.day[ii+1]),
          col=crt.col, lty=crt.lty, lwd=crt.lwd
    )
  }
  lines(c(ldc.crit$ldc.gmn$flow.exceed[4],ldc.crit$ldc.gmn$flow.exceed[5]),
        c(ldc.crit$ldc.gmn$load.day[5],ldc.crit$ldc.gmn$load.day[5]),
        col=crt.col, lty=crt.lty, lwd=crt.lwd
  )
  ## plot ldc points for station
  ldc.col.d="red"
  ldc.col.nd="black"
  ldc.pch.d=19
  ldc.pch.nd=21
  ldc.cex.nd=1.0
  ldc.cex.d=1.2
  ldc.lty="solid"
  ldc.lwd.ds=4
  ldc.lwd.nds=2
  ## plot the ldc geomeans for stations
  if(ldc.stn$geomns$digression[1] == 1) {
    lines(c(0,ldc.stn$geomns$flow.exceed[1]),
          c(ldc.stn$geomns$load.day[1],ldc.stn$geomns$load.day[1]),
          col=ldc.col.d, lty=ldc.lty, lwd=ldc.lwd.ds
    )
  }
  else {
    lines(c(0,ldc.stn$geomns$flow.exceed[1]),
          c(ldc.stn$geomns$load.day[1],ldc.stn$geomns$load.day[1]),
          col=ldc.col.nd, lty=ldc.lty, lwd=ldc.lwd.nds
    )
  }
  for(ii in 2:(length(ldc.stn$geomns$flow.exceed)-1)) {
    if(ldc.stn$geomns$digression[ii] == 1) {
      lines(c(ldc.stn$geomns$flow.exceed[ii-1],ldc.stn$geomns$flow.exceed[ii]),
            c(ldc.stn$geomns$load.day[ii],ldc.stn$geomns$load.day[ii]),
            col=ldc.col.d, lty=ldc.lty, lwd=ldc.lwd.ds
      )
    }
    else {
      lines(c(ldc.stn$geomns$flow.exceed[ii-1],ldc.stn$geomns$flow.exceed[ii]),
            c(ldc.stn$geomns$load.day[ii],ldc.stn$geomns$load.day[ii]),
            col=ldc.col.nd, lty=ldc.lty, lwd=ldc.lwd.nds
      )
    }
  }
  if(ldc.stn$geomns$digression[5] == 1) {
    lines(c(ldc.stn$geomns$flow.exceed[4],ldc.stn$geomns$flow.exceed[5]),
          c(ldc.stn$geomns$load.day[5],ldc.stn$geomns$load.day[5]),
          col=ldc.col.d, lty=ldc.lty, lwd=ldc.lwd.ds
    )
  }
  else {
    lines(c(ldc.stn$geomns$flow.exceed[4],ldc.stn$geomns$flow.exceed[5]),
          c(ldc.stn$geomns$load.day[5],ldc.stn$geomns$load.day[5]),
          col=ldc.col.nd, lty=ldc.lty, lwd=ldc.lwd.nds
    )
  }
  ## plot the digressions as red and the non-digressions as black
  if(sum(ldc.stn$points$digression) > 0) {
    max.ds <- ldc.stn$points[ ldc.stn$points$digression == 1, ]
    max.nds <- ldc.stn$points[ ldc.stn$points$digression == 0, ]
    points(max.ds$flow.exceed,max.ds$load.day,
           col="black", pch=ldc.pch.d, cex=ldc.cex.d
    )
    points(max.ds$flow.exceed,max.ds$load.day,
           col=ldc.col.d, pch=ldc.pch.d, cex=ldc.cex.d*0.8
    )
    points(max.nds$flow.exceed,max.nds$load.day,
           col=ldc.col.nd, pch=ldc.pch.nd, cex=ldc.cex.nd
    )
  }
  else {
    points(ldc.stn$points$flow.exceed,ldc.stn$points$load.day,
           col=ldc.col.nd, pch=ldc.pch.nd, cex=ldc.cex.nd
    )
  }
  if(names(dev.cur()) == "pdf") {
    dev.off()
  }
  options(warn=0)
}
