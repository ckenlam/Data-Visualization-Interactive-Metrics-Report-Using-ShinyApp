library(rCharts)
library(dplyr)
library(shiny)
library(data.table)
library(plyr)
library(dplyr)
library(dygraphs)
library(ggplot2)
library(reshape2)
library(lubridate)


filter_agg_by_inputs <- function(df, mindate, maxdate, teams) {
  # Filter & rearranging the data
  df %>% filter(date_id >= mindate, date_id <= maxdate, Team_Function %in% teams)
  #df<-aggregate(df, by= list(df$month, df$Team_Function, df$app_id, df$app_name), sum, na.rm=TRUE)
}

daplot_calls <- function(DT, yAxisLabel, dom = "plot_calls") {
  daplot_calls <- nPlot(calls ~ date, group = "team", data = DT, type = "multiBarChart", dom = dom)
  daplot_calls$chart(margin = list(left = 80, right = 80))
  daplot_calls$yAxis(axisLabel = yAxisLabel)
  daplot_calls$xAxis(axisLabel = "Day")
  daplot_calls$xAxis( tickFormat="#!function(d) {return d3.time.format('%Y-%m-%d')(new Date((1+d) * 24 * 60 * 60 * 1000));}!#" )
  daplot_calls
}

wplot_calls <- function(DT, yAxisLabel, dom = "plot_calls") {
  wplot_calls <- nPlot(calls ~ date, group = "team", data = DT, type = "multiBarChart", dom = dom)
  wplot_calls$chart(margin = list(left = 80, right = 100))
  wplot_calls$yAxis(axisLabel = yAxisLabel)
  wplot_calls$xAxis(axisLabel = "Weeky of")
  wplot_calls$xAxis( tickFormat="#!function(d) {return d3.time.format('%Y-%m-%d')(new Date((1+d) * 24 * 60 * 60 * 1000));}!#" )
  wplot_calls
}

mplot_calls <- function(DT, yAxisLabel, dom = "plot_calls") {
  mplot_calls <- nPlot(calls ~ date, group = "team", data = DT, type = "multiBarChart", dom = dom)
  mplot_calls$chart(margin = list(left = 80, right = 100))
  mplot_calls$yAxis(axisLabel = yAxisLabel)
  mplot_calls$xAxis(axisLabel = "Month")
  mplot_calls
}

yplot_calls <- function(DT, yAxisLabel, dom = "plot_calls") {
  yplot_calls <- nPlot(calls ~ month, group = "year", data = DT, type = "lineChart", dom = dom)
  yplot_calls$chart(margin = list(left = 80, right = 100))
  yplot_calls$yAxis(axisLabel = yAxisLabel)
  yplot_calls$xAxis(axisLabel = "Month")
  yplot_calls
}

###################################################

daplot_calls_svl <- function(DT, yAxisLabel, dom = "plot_calls") {
  daplot_calls_svl <- nPlot(svl ~ date, group = "team", data = DT, type = "multiBarChart", dom = dom)
  daplot_calls_svl$chart(margin = list(left = 80, right = 80))
  daplot_calls_svl$chart(showControls = FALSE)
  daplot_calls_svl$yAxis(axisLabel = yAxisLabel)
  daplot_calls_svl$yAxis(tickFormat = "#!d3.format('%')!#" )
  daplot_calls_svl$xAxis(axisLabel = "Day")
  daplot_calls_svl$xAxis(tickFormat="#!function(d) {return d3.time.format('%Y-%m-%d')(new Date((1+d) * 24 * 60 * 60 * 1000));}!#")
  daplot_calls_svl
}

wplot_calls_svl <- function(DT, yAxisLabel, dom = "plot_calls") {
  wplot_calls_svl <- nPlot(svl ~ date, group = "team", data = DT, type = "multiBarChart", dom = dom)
  wplot_calls_svl$chart(margin = list(left = 80, right = 100))
  wplot_calls_svl$chart(showControls = FALSE)
  wplot_calls_svl$yAxis(axisLabel = yAxisLabel)
  wplot_calls_svl$yAxis(tickFormat = "#!d3.format('%')!#" )
  wplot_calls_svl$xAxis(axisLabel = "Weeky of")
  wplot_calls_svl$xAxis( tickFormat="#!function(d) {return d3.time.format('%Y-%m-%d')(new Date((1+d) * 24 * 60 * 60 * 1000));}!#" )
  wplot_calls_svl
}

mplot_calls_svl <- function(DT, yAxisLabel, dom = "plot_calls") {
  mplot_calls_svl <- nPlot(svl ~ date, group = "team", data = DT, type = "multiBarChart", dom = dom)
  mplot_calls_svl$chart(margin = list(left = 80, right = 100))
  mplot_calls_svl$chart(showControls = FALSE)
  mplot_calls_svl$yAxis(axisLabel = yAxisLabel)
  mplot_calls_svl$yAxis(tickFormat = "#!d3.format('%')!#" )
  mplot_calls_svl$xAxis(axisLabel = "Month")
  mplot_calls_svl
}

###################################################


#filter_agg_by_inputs2 <- function(df, teams) {
  # Filter & rearranging the data
  #df %>% filter(Team_Function %in% teams)
  #df<-aggregate(df, by= list(df$month, df$Team_Function, df$app_id, df$app_name), sum, na.rm=TRUE)
#}

####################################################
