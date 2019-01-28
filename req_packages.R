####################### Start ########## Automation of package installation ######################################

#If required packages are not present in the machine, than only install them. 

#Packages which are used in the code passed as a string vector
Packages_for_Demand_Simulation <- c("shiny",
                                    "ggplot2",
                                    "plotly",
                                    "gtable",
                                    "grid",
                                    "flextable",
                                    "qtlcharts",
                                    "reshape2",
                                    "ggthemes",              # To apply ggplot themes to the chart
                                    "scales",                # For pretty breaks
                                    "data.table",
                                    "forecast",
                                    "arm",
                                    "dygraphs",
                                    "datasets",
                                    "xts",
                                    "dplyr",
                                    "lubridate")

#Checking which packages are not already installed in the system
Additional_Package <- Packages_for_Demand_Simulation[!(Packages_for_Demand_Simulation 
                                                       %in% installed.packages()[,"Package"])]

#If atleast one package is not already installed in the system install them
if(length(Additional_Package)) install.packages(Additional_Package)

  
  
  


library(shiny)
library(ggplot2)
library(plotly)
library(gtable)
library(grid)
library(qtlcharts)
library(reshape2)
library(ggthemes)              # To apply ggplot themes to the chart
library(scales)                # For pretty breaks
library(data.table)
library(forecast)
library(arm)
library(dygraphs)
library(datasets)
library(xts)
library(dplyr)
library(lubridate)

# if (require("rCharts")==FALSE){
#   devtools::install_local(path = 
#                             paste0(getwd(),
#                                    "/packages/ramnathv-rCharts-2c368c8",
#                                    collapse = ""))
# }
# library("rCharts")