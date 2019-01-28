library(dygraphs)
library(shinythemes)
shinyUI(fluidPage(
  theme = shinytheme("cosmo"),
  titlePanel("Forecast Platform"),
  
  navbarPage(
    title="",id = "tabs",
    tabPanel("Load Data",value = "ld_data",
             uiOutput('page.LD')
    ),
    tabPanel("FB Prophet", value = "fb_prophet",
             uiOutput('page.fc')),
    tabPanel("ARIMAX", value = "arimax",
             uiOutput('page.ar'))
    )))