page_forecast_arimax.UI<-renderUI({
  sidebarLayout(
    sidebarPanel(
      selectInput("agg_type_ari", label = "Select type of aggregation/filtering",
                  choices = c("12-hour","12-hour (Weekdays)","Daily",
                              "Weekdays","Weekly (Weekdays)"),
                  selected="Daily"),
      uiOutput("FC.time.var.ari"),
      uiOutput("FC.fore.var.ari"),
      textInput("season.ari", label = "Type the Seasonality Name",
                value = "Auto"),
      fluidRow(
        
        column(6,
               uiOutput("FC.season.period.ari")       
        ),
        column(6,
               uiOutput("FC.season.fourier.ari")
        )),
      fluidRow(
        
        column(6,actionButton("FC.btn.season.ari",
                              label="Add")),
        column(6,checkboxInput("use_previous_ari", label = "Use Previous", value = TRUE))),
      sliderInput("p_ari", label = "Number of autoregressive terms (p)",
                  min = 0, max = 10, step = 1,
                  value = 1),
      sliderInput("d_ari", label = "Number of nonseasonal differences (d)",
                  min = 0, max = 10, step = 1,
                  value = 1),
      sliderInput("q_ari", label = "Number of lagged forecast errors (q)",
                  min = 0, max = 10, step = 1,
                  value = 1),
      uiOutput("FC.holiday.ari"),
      uiOutput("FC.date.range.ari"),
      numericInput("time_period_ari", label = "Time periods to Predict", 
                   value = 3, min = 1, max = 200, step = 1),
      selectInput("interval_ari", label = "Prediction Interval",
                  choices = c("0.80", "0.90", "0.95", "0.99"),
                  selected = "0.95"),
      checkboxInput("showgrid_ari", label = "Show Grid", value = TRUE),
      hr(),
      actionButton("pg.btn.forecast.update.ari",
                   label="Forecast"),
      hr(),
      div(strong("From: "), textOutput("from_ari", inline = TRUE)),
      div(strong("To: "), textOutput("to_ari", inline = TRUE)),
      br(),
      helpText("Click and drag to zoom in (double click to zoom back out).")
    ),
    mainPanel(
      fluidRow(dygraphOutput("dygraph_ari")),
      hr(),
      # h4("Accuracy Table"),
      fluidRow(column(6,"Accuracy Table",tableOutput("acctable_ari")),
               column(6,"Seasonality Used",dataTableOutput("seasontable_ari"))),
      hr(),
      fluidRow(column(4,selectInput("error_chart_ari", label = "Select type of Chart",
                                    choices = c("Percentage Error","Absolute Error"),
                                    selected="Percentage Error"))),
      fluidRow(dygraphOutput("dygraph_err_ari")),
      hr(),
      fluidRow(column(width = 6,numericInput("cut_off_ari", label = "Number of increment points for validation", 
                                             value = 10, min = 1, max = 30, step = 1)),
               column(width = 3,textInput("naive_period_ari", label="Period to calculate MASE", value = 1)),
               column(width = 3,actionButton("pg.btn.kfold.update.ari",
                                             label="K-Fold Validate"))),
      h4("K-Fold Validation Table"),
      fluidRow(dataTableOutput("kfoldtable_ari")
      )
    )
    
  )
}
)

FC.UI.Time.Var.Ari<-function(df){
  ui<-renderUI(selectInput("time_var_Ari", label = "Select Time Variable",
                           choices = c("Select",unique(colnames(df))),
                           selected="Select")) 
  return(ui)
}

FC.UI.Fore.Var.Ari<-function(df){
  ui<-renderUI(selectInput("fore_var_Ari", label = "Select Forecast Variable",
                           choices = c("Select",unique(colnames(df))),
                           selected="Select")) 
  return(ui)
}

FC.season_period.ari<-function (input){
  ui<-renderUI(textInput("season.period.ari", label = "Period of the Seasonality", value = 
                           if(input$season.ari=="Weekly")
                           {7}
                         else if(input$season.ari=="Monthly")
                         {30.4}
                         else if(input$season.ari=="Quarterly")
                         {91.3125}
                         else if(input$season.ari=="Yearly")
                         {365.25}
                         else if(input$season.ari=="Daily")
                         {1}
                         else
                           ""))
  return(ui)
}

FC.season_fourier.ari<-function (input){
  ui<-renderUI(textInput("season.fourier.ari", label = "Fourier order of the Seasonality", value = 
                           if(input$season.ari=="Weekly")
                           {3}
                         else if(input$season.ari=="Monthly")
                         {5}
                         else if(input$season.ari=="Quarterly")
                         {7}
                         else if(input$season.ari=="Yearly")
                         {10}
                         else if(input$season.ari=="Daily")
                         {2}
                         else
                           ""))
  return(ui)
}

FC.UI.Holiday.ari<-function(input,df_list){

  ui<-renderUI(
    if(input$agg_type_ari=="Weekly (Weekdays)")
    {
      df<-df_list[[4]]
      if(!is.null(df))
      {
        selectInput("holiday.ari", label = "Select Holiday/Events",
                    choices = c("None",colnames(df)),multiple = TRUE,
                    selected="None")
      }
      else 
      {
        selectInput("holiday.ari", label = "Select Holiday/Events",
                    choices = "None",multiple = TRUE,
                    selected="None")
      }
    }
    else if(input$agg_type_ari=="12-hour (Weekdays)")
    {
      df<-df_list[[6]]
      if(!is.null(df))
      {
        selectInput("holiday.ari", label = "Select Holiday/Events",
                    choices = c("None",colnames(df)),multiple = TRUE,
                    selected="None")
      }
      else 
      {
        selectInput("holiday.ari", label = "Select Holiday/Events",
                    choices = "None",multiple = TRUE,
                    selected="None")
      }
    }
    else
    {
      df<-df_list[[2]]
      if(!is.null(df))
      {
        selectInput("holiday.ari", label = "Select Holiday/Events",
                    choices = c("None",colnames(df)),multiple = TRUE,
                    selected="None")
      }
      else
      {
        selectInput("holiday.ari", label = "Select Holiday/Events",
                    choices = "None",multiple = TRUE,
                    selected="None")
      }
    }
  )
  
  return(ui)
}

FC.UI.Training.Data.ari<-function(input,df){
  ui<-renderUI(dateRangeInput('dateRange_ari',
                              label = 'Training Date range input: yyyy-mm-dd',
                              start = as.Date("1970-01-01")+ifelse(input$time_var_Ari=='Select',(Sys.Date() - 2),
                                                                   min(lubridate::date(df[,input$time_var_Ari]))),
                              end = as.Date("1970-01-01")+ifelse(input$time_var_Ari=='Select',(Sys.Date() + 2),
                                                                 max(lubridate::date(df[,input$time_var_Ari])))
  ))
  return(ui)
}