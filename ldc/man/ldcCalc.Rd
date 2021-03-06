% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ldcCalc.R
\name{ldcCalc}
\alias{ldcCalc}
\title{LDC CALC}
\usage{
ldcCalc(df.stn.ecoli, df.stn.flow)
}
\arguments{
\item{df.stn.ecoli, }{dataframe: bacteria data for indivdual station-- c('Date', 'value')}

\item{df.stn.flow, }{dataframe: flow data for individual station-- c('date', 'value', 'flow.exceed')}
}
\value{
list containing four dataframes: points, summary.points, gemomns, and summary.geomns
}
\description{
This function will output a list containing:
points: dataframe which includes the date, ecoli result, digression and reduction value, flow, vol per day, flow exceedance, flow zone (transitional, dry, etc), and load/day
summary.points: dataframe summary of points dataframe which includes, N, digressions, max reductions, max reuction flows, max reduction flow exceed, and max reduction ecoli
geomns: dataframe which includes flow exceedance, vol/day, concentration, N, digression and reduction, and flow zone description
summary.geomns: summary of geomns dataframe, including N, Digressions, Max reduction, Max reduction flows, and max reduction concentration
}
