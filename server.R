options(shiny.maxRequestSize=100*1024^2)

source('req_packages.R',local=TRUE)
source('main_functions.R',local=TRUE)
source('page_loaddata.R',local=TRUE)
source('page_forecast.R',local=TRUE)
source('page_forecast_ari.R',local=TRUE)

library(dygraphs)
library(forecast)
library(datasets)
library(Rcpp)
library(prophet)
library(xts)
library(flextable)
library(dplyr)

shinyServer(function(input, output) {
  
  # Reactive Values for uploaded dataset
  values <- reactiveValues()
  values$df_data<-NULL
  values$df_data_orig<-NULL
  values$df_str<-NULL
  values$dy_graph_df<-NULL
  values$dy_graph_df_ar<-NULL
  values$dy_graph<-NULL
  values$dy_graph_ar<-NULL
  values$model_fb<-NULL
  values$error_result<-NULL
  values$error_result_ar<-NULL
  values$error_graph<-NULL
  values$acc_tb<-NULL
  values$acc_tb_ar<-NULL
  values$model_pred<-NULL
  values$work_df<-NULL
  values$hol_df<-NULL
  values$k_fold_table<-NULL
  values$k_fold_table_ar<-NULL
  values$hol_df_proc<-NULL
  values$season_df<-NULL
  values$season_df_ar<-NULL
  values$arima_regressors<-NULL
  values$season_df_temp<-NULL
  values$season_df_temp_ar<-NULL
  
  # Actions done when file input button is clicked
  observeEvent(input$file1, {
    # Function to import the data
    values$df_data_orig <- s_data(input,output)
    values$df_data <- values$df_data_orig
    # The summary of the data imported
    values$df_str<-df_datasummary(input,output,values$df_data)
    
    # Variables to store datatype values
    output$LD.var <- page_LD.UI.var(values$df_data)
    output$LD.datatype <- page_LD.UI.datatype(input)
    output$LD.format <- page_LD.UI.format(input)
    output$LD.cust.format<-page_LD.UI.format_TB(input)
    
  })
  
  # Actions done when datatype action button is clicked
  observeEvent(input$pg.btndt.update, {
    
    # Function to update class of the variable selected
    temp <- transform_class(input,values$df_data,values$df_data_orig)
    values$df_data <- temp
    
    # Updating the summary of the data accordingly
    values$df_str<-df_datasummary(input,output,values$df_data)
    
    # Variables to store datatype values
    output$LD.var <- page_LD.UI.var(values$df_data)
    output$LD.datatype <- page_LD.UI.datatype(input)
    output$LD.format <- page_LD.UI.format(input)
    output$LD.cust.format<-page_LD.UI.format_TB(input)
    
    output$FC.time.var <- FC.UI.Time.Var(values$df_data)
    output$FC.time.var.ari <- FC.UI.Time.Var.Ari(values$df_data)
    output$FC.fore.var <- FC.UI.Fore.Var(values$df_data)
    output$FC.fore.var.ari <- FC.UI.Fore.Var.Ari(values$df_data)
    output$FC.date.range <- FC.UI.Training.Data(input,values$df_data)
    output$FC.date.range.ari <- FC.UI.Training.Data.ari(input,values$df_data)
    
  })
  
  # Actions done when Export action button is clicked
  observeEvent(input$pg.btnExportFile,
               s_export_data(values$df_data))
  
  ##############################################################
  # Page  - Load Data
  output$page.LD <- page.LoadData.UI
  output$page.fc <- page_forecast.UI
  output$page.ar <- page_forecast_arimax.UI
  
  # Output Table results
  output$table <- renderDataTable({
    values$df_data
  })
  
  # Output data summary
  output$table_str<-renderDataTable({
    values$df_str
  })
  
  # Update the datatype
  output$LD.update.table<-pg.update_table()
  
  # Export the file
  output$export_file<-pg.loadData.export_file()
  
#############################################################################
  
  
############################# Forecast ######################################  
  
  values$hol_df<-fc_holiday_df()
  
  # Actions done when season add action button is clicked
  observeEvent(input$FC.btn.season,
               values$season_df<-seasonality_df(input,values$season_df))
  observeEvent(input$FC.btn.season.ari,
               values$season_df_ar<-seasonality_df(input,values$season_df_ar))
  
  observeEvent(input$pg.btn.forecast.update,
               {
                 if(input$use_previous)
                 {
                   values$season_df<-values$season_df_temp
                 }
                 
                 values$df_data_orig_proc<-fc_agg_df(input,values$df_data,
                                                     values$df_data_orig)
                 values$df_data_proc<-agg_transform(input,values$df_data_orig_proc)
                 
                 values$work_df<-fc_work_df(input,values$df_data_proc)
                 values$hol_df_proc<-fc_hol_df(input,values$hol_df)
                 
                 if(input$tabs=="fb_prophet")
                 {
                   values$model_fb<-fc_model(input,values$work_df,values$hol_df_proc,
                                             values$season_df)
                   values$model_pred <- predicted_model(input,values$model_fb,
                                                        values$work_df,
                                                        values$df_data_proc)
                 }
                 
                 values$dy_graph_df<-pro_forecast_df(input,values$work_df,
                                                     values$df_data_proc,
                                                     values$df_data_orig_proc,
                                                     values$model_pred)
                 
                 values$dy_graph<-pro_forecast_func(input,values$dy_graph_df)
                 
                 values$error_result<-error_table(input,
                                                  values$work_df,
                                                  values$model_pred,
                                                  values$df_data_proc,
                                                  values$df_data_orig_proc)
                 values$acc_tb<-values$error_result[[1]]
                 # values$error_graph<-error_chart(input,values$error_result)
                 values$season_df_temp<-values$season_df
                 values$season_df<-NULL
                 })
  
  observeEvent(input$pg.btn.forecast.update.ari,
               {
                 if(input$use_previous_ari)
                 {
                   values$season_df_ar<-values$season_df_temp_ar
                 }
                 
                 values$df_data_orig_proc<-fc_agg_df(input,values$df_data,
                                                     values$df_data_orig)
                 values$df_data_proc<-agg_transform(input,values$df_data_orig_proc)
                 
                 values$work_df<-fc_work_df(input,values$df_data_proc)
                 values$hol_df_proc<-fc_hol_df(input,values$hol_df)
                 
                 values$arima_regressors<-arima_regressors(values$df_data_proc,
                                                           values$season_df_ar,
                                                           values$hol_df_proc)
                 
                 values$model_fb<-fc_model_ar(input,values$work_df,values$arima_regressors)
                
                 values$model_pred <- predicted_model_ar(input,values$model_fb,
                                                        values$work_df,
                                                        values$arima_regressors)
                 
                 values$dy_graph_df_ar<-pro_forecast_df(input,values$work_df,
                                                       values$df_data_proc,
                                                    values$df_data_orig_proc,
                                                    values$model_pred)
                 
                 values$dy_graph_ar<-pro_forecast_func(input,values$dy_graph_df_ar)
                   
                 values$error_result_ar<-error_table(input,values$work_df,
                                                  values$model_pred,values$df_data_proc,
                                                  values$df_data_orig_proc)
                 values$acc_tb_ar<-values$error_result_ar[[1]]
                 # values$error_graph<-error_chart(input,values$error_result)
                 values$season_df_temp_ar<-values$season_df_ar
                 values$season_df_ar<-NULL
               })
  
  observeEvent(input$pg.btn.kfold.update,
               {
                 values$k_fold_table<-n_fold_validation(input,values$df_data_proc,
                                                        values$df_data_orig_proc,
                                                        values$hol_df_proc,
                                                        values$season_df_temp)
               })
  observeEvent(input$pg.btn.kfold.update.ari,
               {
                 values$k_fold_table_ar<-n_fold_validation(input,values$df_data_proc,
                                                        values$df_data_orig_proc,
                                                        values$hol_df_proc,
                                                        values$season_df_temp_ar)
               })
  
  
  output$FC.aggregation <- FC.UI.aggregation()
  output$FC.season <- FC.UI.Season()
  output$FC.season.period <- FC.season_period(input)
  output$FC.season.period.ari <- FC.season_period.ari(input)
  output$FC.season.fourier <- FC.season_fourier(input)
  output$FC.season.fourier.ari <- FC.season_fourier.ari(input)
  output$FC.season.add <- FC.add_season()
  output$FC.holiday <- FC.UI.Holiday(input,values$hol_df)
  output$FC.holiday.ari <- FC.UI.Holiday.ari(input,values$hol_df)
  output$FC.cut.off <- FC.UI.Cut.Off()
  output$FC.update.model <- pg.update_forecast()
  output$FC.update.kfold<-pg.update_k_fold()
  
  output$dygraph <- renderDygraph(values$dy_graph)
  output$dygraph_ari <- renderDygraph(values$dy_graph_ar)
  output$dygraph_err <- renderDygraph(error_chart(input,values$error_result))
  output$dygraph_err_ari <- renderDygraph(error_chart(input,values$error_result_ar))
  output$acctable <- renderTable(values$acc_tb
                                 # ,options = list(searching = FALSE,paging = FALSE)
                                 )
  output$acctable_ari <- renderTable(values$acc_tb_ar
                                 # ,options = list(searching = FALSE,paging = FALSE)
  )
  output$seasontable <- renderDataTable(values$season_df_temp
                                       ,options = list(searching = FALSE,paging = FALSE))
  output$seasontable_ari <- renderDataTable(values$season_df_temp_ar
                                        ,options = list(searching = FALSE,paging = FALSE))
  output$FC.MASE.type <- FC.UI.MASE.type()
  
  output$kfoldtable <- renderDataTable(values$k_fold_table
                                 ,options = list(searching = FALSE,paging = FALSE)
                                 
                                 
  )
  output$kfoldtable_ari <- renderDataTable(values$k_fold_table_ar
                                       ,options = list(searching = FALSE,paging = FALSE))
  # output$kfoldtable_MAPE <- renderDataTable(k_fold_table_selector_MAPE(input,values$k_fold_table)
  #                                      ,options = list(searching = FALSE,paging = FALSE)
  #                                      
  # )
  
  output$from <- renderText({
    strftime(req(input$dygraph_date_window[[1]]), "%d %b %Y")      
  })
  
  output$from_ari <- renderText({
    strftime(req(input$dygraph_ari_date_window[[1]]), "%d %b %Y")      
  })
  
  output$to <- renderText({
    strftime(req(input$dygraph_date_window[[2]]), "%d %b %Y")
  })
  output$to_ari <- renderText({
    strftime(req(input$dygraph_ari_date_window[[2]]), "%d %b %Y")
  })

})
