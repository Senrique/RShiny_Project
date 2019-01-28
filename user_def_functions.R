page_lm.pred_plot<-function(input,df,mod.fit){
  if (is.null(df))
    return(NULL)
  
  if (is.null(mod.fit))
    return(NULL)
  
  plot_df<-data.frame(cbind(df$Week_ending,
                            df[,input$pg_lm.dep],
                            predict.lm(mod.fit,
                                       newdata=df[,input$pg_lm.ind])))
  names(plot_df)<-c("Date","Actual","Predicted")
  
  xt_data<-xts(plot_df[1:nrow(plot_df),c(2,3)],
               order.by=as.Date(plot_df$Date))
  
  plt<-dygraph(xt_data) %>% 
    dyRangeSelector(height=20) %>% 
    dyOptions(stepPlot = TRUE,
              axisLineColor = "grey",
              axisLabelColor = "white",
              colors= c('#c9c9ff','#ffbdbd')) %>% 
    dyLegend(width = 400)
  
  
  return(plt)
}


# Shared functions 
s_data <- function(input,output) {
  inFile <- input$file1
  if (is.null(inFile))
    return(NULL)
  # table<-read.csv(inFile$datapath, header = input$header,
  #                 sep = input$sep, quote = input$quote,stringsAsFactors = FALSE)
  table<-as.data.frame(fread(inFile$datapath,header = input$header,sep = input$sep))
  return(table)    
  
}

df_datasummary<-function(input,output,df){
  varName<-names(df)
  varType<-sapply(df,class)
  varCount<-apply(df,2,function(x) length(x))
  varUnique<-apply(df,2,function(x) length(unique(x)))
  varNA<-apply(df,2,function(x) sum(is.na(x)))
  varBlanks<-apply(df,2,function(x) sum(ifelse((x==""),1,0)))
  df_str<-data.frame(cbind(varName,varType
                           ,varCount,
                           varUnique,varNA,varBlanks))
  names(df_str)<-c('Variable','Type','Count','Unique',
                   'NAs','Blanks')
  return(df_str)
}

s_export_data<-function(df){
  write.csv(df,"DataFile.csv",row.names = FALSE)
}

s_lm.mod <- function(input, output,data) {
  tryCatch({
    df<-data[trainRows(input,data),]
    eq<-input$pg_lm.lm_eq
    fit<-lm(eq,data=df)
    return(fit)
  }, error = function(e) {
    return(NULL)
  }
  )
}

df_datasummary<-function(input,output,df){
  varName<-names(df)
  varType<-sapply(df,class)
  varCount<-apply(df,2,function(x) length(x))
  varUnique<-apply(df,2,function(x) length(unique(x)))
  varNA<-apply(df,2,function(x) sum(is.na(x)))
  varBlanks<-apply(df,2,function(x) sum(ifelse((x==""),1,0)))
  df_str<-data.frame(cbind(varName,varType
                           ,varCount,
                           varUnique,varNA,varBlanks))
  names(df_str)<-c('Variable','Type','Count','Unique',
                   'NAs','Blanks')
  return(df_str)
}

transform_class<-function(input,df){
  tryCatch({if(input$pg_LD.var!='Select'){
    var_to_change<-df[,input$pg_LD.var]
    if(input$pg_LD.DT=='Character'){
      df[,input$pg_LD.var]<-as.character(var_to_change)
    }
    else if(input$pg_LD.DT=='Integer'){
      df[,input$pg_LD.var]<-as.integer(var_to_change)
    }
    else if(input$pg_LD.DT=='Numeric'){
      df[,input$pg_LD.var]<-as.numeric(var_to_change)
    }
    else if(input$pg_LD.DT=='Factor'){
      df[,input$pg_LD.var]<-as.factor(var_to_change)
    }
    else if(input$pg_LD.form.tb!=""){
      df[,input$pg_LD.var]<-as.data.frame(strptime(var_to_change,format=input$pg_LD.form.tb))
    }
    else
      df[,input$pg_LD.var]<-var_to_change
    return(df)
  }
  else return(df)}, error = function(e) {
    return(df)
  }
  )
}

pro_forecast_func<-function(input,df){
  predicted <- reactive({
    
    work_df<-df[,c(input$time_var,input$fore_var)]
    colnames(work_df)<-c("ds","y")
    fbp <- prophet(work_df,interval.width = as.numeric(input$interval))
    future <- make_future_dataframe(fbp, periods = input$time_period)
    Predicted<-predict(fbp, future)
    
    Actual<-Predicted$y
    Actual<-c(work_df$y,rep(NA,(nrow(future)-nrow(work_df))))
    predicted.fit<-Predicted$yhat
    predicted.lwr<-Predicted$yhat_lower
    predicted.upr<-Predicted$yhat_upper
    
    xts(cbind(Actual,predicted.fit,predicted.lwr,predicted.upr),
        order.by=future$ds)
  })
  
  dygraph(predicted(), main = "Forecast of orders") %>%
    dySeries(paste0("predicted.", c("lwr", "fit", "upr")), label = "Predicted") %>%
    dyRangeSelector(height=20) %>%
    dyOptions(drawGrid = input$showgrid)
}

# Function to calculate accuracy/error
error_table<-function(input,df){
  m <- prophet(df,yearly.seasonality = T,weekly.seasonality = T)
  future <- make_future_dataframe(m, periods = 365)
  fp <- predict(m, future)[,c('yhat')]
  fp_fit <- ts(fp[1:(length(fp)-364)],frequency = 365)
  fp_hist <- msts(m$history[,'y'],seasonal.periods = c(7,365.25))
  fp_mean <- ts(fp[length(fp)-364:0],frequency = 365,start = end(fp_hist))
  fprophet <- list(mean = fp_mean,fitted = fp_fit,x = fp_hist)
  class(fprophet) <- "forecast"
  accuracy(fprophet)
}
# m <- prophet(df,yearly.seasonality = T,weekly.seasonality = T)
# future <- make_future_dataframe(m, periods = 365)
# fp <- predict(m, future)[,c('yhat')]
# fp_fit <- ts(fp[1:(length(fp)-364)],frequency = 365)
# fp_hist <- msts(m$history[,'y'],seasonal.periods = c(7,365.25))
# fp_mean <- ts(fp[length(fp)-364:0],frequency = 365,start = end(fp_hist))
# fprophet <- list(mean = fp_mean,fitted = fp_fit,x = fp_hist)
# class(fprophet) <- "forecast"
# accuracy(fprophet)
