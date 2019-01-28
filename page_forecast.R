page_forecast.UI<-renderUI({
  sidebarLayout(
  sidebarPanel(
    uiOutput("FC.aggregation"),
    uiOutput("FC.time.var"),
    uiOutput("FC.fore.var"),
    uiOutput("FC.season"),
    fluidRow(
      
      column(6,
             uiOutput("FC.season.period")       
      ),
      column(6,
             uiOutput("FC.season.fourier")
      )),
    fluidRow(
      
      column(6,uiOutput("FC.season.add")),
      column(6,checkboxInput("use_previous", label = "Use Previous", value = TRUE))),
    textInput("season_scale", label="Seasonality Weight", value = 10),
    textInput("n_changepoints", label="Number of change points in the series", value = 25),
    textInput("changepoints_scale", label="Changepoints Weight", value = 0.05),
    uiOutput("FC.holiday"),
    textInput("holiday_scale", label="Holiday Weight", value = 10),
    textInput("mcmc_samples", label="Number of MCMC samples for Bayesian Inference",
              value = 0),
    textInput("uncertainty_samples", label="Number of Simulations for uncertainty estimations",
              value = 1000),
    uiOutput("FC.date.range"),
    numericInput("time_period", label = "Time periods to Predict", 
                 value = 6, min = 1, max = 200, step = 1),
    selectInput("interval", label = "Prediction Interval",
                choices = c("0.80", "0.90", "0.95", "0.99"),
                selected = "0.95"),
    checkboxInput("showgrid", label = "Show Grid", value = TRUE),
    hr(),
    uiOutput("FC.update.model"),
    hr(),
    div(strong("From: "), textOutput("from", inline = TRUE)),
    div(strong("To: "), textOutput("to", inline = TRUE)),
    br(),
    helpText("Click and drag to zoom in (double click to zoom back out).")
  ),
  mainPanel(
    fluidRow(dygraphOutput("dygraph")),
      hr(),
      # h4("Accuracy Table"),
      fluidRow(column(6,"Accuracy Table",tableOutput("acctable")),
               column(6,"Seasonality Used",dataTableOutput("seasontable"))),
    hr(),
    fluidRow(column(4,selectInput("error_chart", label = "Select type of Chart",
                         choices = c("Percentage Error","Absolute Error"),
                         selected="Percentage Error"))),
    fluidRow(dygraphOutput("dygraph_err")),
    hr(),
      fluidRow(column(width = 6,uiOutput("FC.cut.off")),
               column(width = 3,uiOutput("FC.MASE.type")),
               column(width = 3,uiOutput("FC.update.kfold"))),
      h4("K-Fold Validation Table"),
      fluidRow(dataTableOutput("kfoldtable")
      )
  )

)
}
)

pg.update_forecast<-function(){
  ui<-renderUI(actionButton("pg.btn.forecast.update",
                            label="Forecast"))
  return(ui)
}

pg.update_k_fold<-function(){
  ui<-renderUI(actionButton("pg.btn.kfold.update",
                            label="K-Fold Validate"))
  return(ui)
}

FC.UI.aggregation<-function(){
  ui<-renderUI(selectInput("agg_type", label = "Select type of aggregation/filtering",
                           choices = c("12-hour","12-hour (Weekdays)","Daily",
                                       "Weekdays","Weekly (Weekdays)"),
                           selected="Daily")) 
  return(ui)
}

FC.UI.Time.Var<-function(df){
  ui<-renderUI(selectInput("time_var", label = "Select Time Variable",
            choices = c("Select",unique(colnames(df))),
            selected="Select")) 
  return(ui)
  }

FC.UI.Fore.Var<-function(df){
    ui<-renderUI(selectInput("fore_var", label = "Select Forecast Variable",
            choices = c("Select",unique(colnames(df))),
            selected="Select")) 
    return(ui)
}

FC.add_season<-function(){
  ui<-renderUI(actionButton("FC.btn.season",
                            label="Add"))
  return(ui)
}

FC.season_name<-function (input){
  ui<-renderUI(textInput("season.name", label = "Name of the Seasonality", value = 
                           if(input$season=="Weekly")
                           {"Weekly"}
                         else if(input$season=="Monthly")
                         {"Monthly"}
                         else if(input$season=="Quarterly")
                         {"Quarterly"}
                         else if(input$season=="Yearly")
                         {"Yearly"}
                         else if(input$season=="Daily")
                         {"Daily"}
                         else if(input$season=="Custom")
                         {""}
                         else
                           ""))
  return(ui)
}

FC.season_period<-function (input){
  ui<-renderUI(textInput("season.period", label = "Period of the Seasonality", value = 
                           if(input$season=="Weekly")
                           {7}
                         else if(input$season=="Monthly")
                         {30.4}
                         else if(input$season=="Quarterly")
                         {91.3125}
                         else if(input$season=="Yearly")
                         {365.25}
                         else if(input$season=="Daily")
                         {1}
                         else
                           ""))
  return(ui)
}

FC.season_fourier<-function (input){
  ui<-renderUI(textInput("season.fourier", label = "Fourier order of the Seasonality", value = 
                           if(input$season=="Weekly")
                           {3}
                         else if(input$season=="Monthly")
                         {5}
                         else if(input$season=="Quarterly")
                         {7}
                         else if(input$season=="Yearly")
                         {10}
                         else if(input$season=="Daily")
                         {2}
                         else
                           ""))
  return(ui)
}

FC.UI.Season<-function(){
  ui<-renderUI(textInput("season", label = "Type the Seasonality Name",
                           value = "Auto")) 
  return(ui)
}

FC.UI.Holiday<-function(input,df_list){

    ui<-renderUI(
      if(input$agg_type=="Weekly (Weekdays)")
      {
        df<-df_list[[3]]
        if(!is.null(df))
        {
          selectInput("holiday", label = "Select Holiday/Events",
                      choices = c("None",unique(df$holiday)),multiple = TRUE,
                      selected="None")
        }
        else
        {
          selectInput("holiday", label = "Select Holiday/Events",
                      choices = "None",multiple = TRUE,
                      selected="None")
        }
      }
      else if(input$agg_type=="12-hour (Weekdays)")
      {
        df<-df_list[[5]]
        if(!is.null(df))
        {
          selectInput("holiday", label = "Select Holiday/Events",
                      choices = c("None",unique(df$holiday)),multiple = TRUE,
                      selected="None")
        }
        else
        {
          selectInput("holiday", label = "Select Holiday/Events",
                      choices = "None",multiple = TRUE,
                      selected="None")
        }
      }
      else
      {
        df<-df_list[[1]]
        if(!is.null(df))
        {
          selectInput("holiday", label = "Select Holiday/Events",
                      choices = c("None",unique(df$holiday)),multiple = TRUE,
                      selected="None")
        }
        else
        {
          selectInput("holiday", label = "Select Holiday/Events",
                      choices = "None",multiple = TRUE,
                      selected="None")
        }
      }
      
      )

  return(ui)
}

FC.UI.Training.Data<-function(input,df){
  ui<-renderUI(dateRangeInput('dateRange',
               label = 'Training Date range input: yyyy-mm-dd',
               start = as.Date("1970-01-01")+ifelse(input$time_var=='Select',(Sys.Date() - 2),
                              min(lubridate::date(df[,input$time_var]))),
               end = as.Date("1970-01-01")+ifelse(input$time_var=='Select',(Sys.Date() + 2),
                            max(lubridate::date(df[,input$time_var])))
  ))
  return(ui)
}

FC.UI.Season.scale<-function(){
  ui<-renderUI(textInput("season_scale", label="Seasonality Scale", value = 10)) 
  return(ui)
}

FC.UI.Cut.Off<-function(){
  ui<-renderUI(numericInput("cut_off", label = "Number of increment points for validation", 
                            value = 10, min = 1, max = 30, step = 1))
  return(ui)
}

FC.UI.MASE.type<-function(){
  ui<-renderUI(textInput("naive_period", label="Period to calculate MASE", value = 1))
  return(ui)
}