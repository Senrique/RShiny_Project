# To Input the data
s_data <- function(input,output) {
  inFile <- input$file1
  if (is.null(inFile))
    return(NULL)
  table<-as.data.frame(fread(inFile$datapath,header = input$header,sep = input$sep))
  return(table)    
  
}

# To export the transformed data
s_export_data<-function(df){
  write.csv(df,"DataFile.csv",row.names = FALSE)
}

# Function to output the table of summary of the data inputted
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

# Function to change class of the variable selected
transform_class<-function(input,df,df_orig){
  tryCatch({if(input$pg_LD.var!='Select'){
    var_to_change<-df[,input$pg_LD.var]
    var_pos<-match(input$pg_LD.var,colnames(df))
    
    log_transform<-max(gregexpr(pattern ='_log_',input$pg_LD.var)[[1]])
    pwr_transform<-max(gregexpr(pattern ='_pwr_',input$pg_LD.var)[[1]])
    
    div_pos<-max(gregexpr(pattern ="/",input$pg_LD.form.tb)[[1]])
    if(div_pos>0)
    {
      numerator<-as.numeric(substr(input$pg_LD.form.tb,1,
                                   div_pos-1))
      
      denominator<-as.numeric(substr(input$pg_LD.form.tb,div_pos+1,
                                     nchar(input$pg_LD.form.tb)))
    }
    
    if(log_transform>0)
    {
      var_to_change<-df_orig[,match(substr(input$pg_LD.var,1,log_transform-1),
                                    colnames(df_orig))]
      colnames(df)[var_pos]<-substr(input$pg_LD.var,1,log_transform-1)
    }
    if(pwr_transform>0)
    {
      var_to_change<-df_orig[,match(substr(input$pg_LD.var,1,pwr_transform-1),
                                    colnames(df_orig))]
      colnames(df)[var_pos]<-substr(input$pg_LD.var,1,pwr_transform-1)
    }
    
    if(input$pg_LD.DT=='Character'){
      df[,var_pos]<-as.character(var_to_change)
    }
    else if(input$pg_LD.DT=='Integer'){
      df[,var_pos]<-as.integer(var_to_change)
    }
    else if(input$pg_LD.DT=='Numeric'){
      df[,var_pos]<-as.numeric(var_to_change)
    }
    else if(input$pg_LD.DT=='Factor'){
      df[,var_pos]<-as.factor(var_to_change)
    }
    else if(input$pg_LD.DT=='Log'){
      if(input$pg_LD.format!='Numeric')
      {
        var_to_change<-as.numeric(as.character(var_to_change))
      }
      if(input$pg_LD.form.tb=="e")
      {
        var_to_change<-ifelse(var_to_change==0,1,var_to_change)
        df[,var_pos]<-log(var_to_change)
      }
      else
        if(div_pos>0)
        {
          var_to_change<-ifelse(var_to_change==0,1,var_to_change)
          df[,var_pos]<-log(var_to_change,base = numerator/denominator)
        }
      else
        df[,var_pos]<-log(var_to_change,base = as.numeric(input$pg_LD.form.tb))
      
      colnames(df)[var_pos]<-paste0(colnames(df)[var_pos],"_log_",input$pg_LD.form.tb)
    }
    else if(input$pg_LD.DT=='Power'){
      if(input$pg_LD.format!='Numeric')
      {
        var_to_change<-as.numeric(as.character(var_to_change))
      }
      if(input$pg_LD.form.tb=="1")
        df[,var_pos]<-var_to_change
      else if(div_pos>0)
      {
        df[,var_pos]<-var_to_change^(numerator/denominator)
        colnames(df)[var_pos]<-paste0(colnames(df)[var_pos],"_pwr_",input$pg_LD.form.tb)
      }
      else
      {
        df[,var_pos]<-var_to_change^as.numeric(input$pg_LD.form.tb)
        colnames(df)[var_pos]<-paste0(colnames(df)[var_pos],"_pwr_",input$pg_LD.form.tb)
      }
      
    }
    else if(input$pg_LD.form.tb!=""&input$pg_LD.DT=='Date'){
      df[,var_pos]<-as.data.frame(strptime(var_to_change,format=input$pg_LD.form.tb,
                                           tz = "America/New_York"))
    }
    else
      df[,var_pos]<-var_to_change
    return(df)
  }
    else return(df)}, error = function(e) {
      return(df)
    }
  )
}

agg_transform<-function(input,df)
{
  if(input$tabs=="fb_prophet")
    fore_var<-input$fore_var
  else
    fore_var<-input$fore_var_Ari

  log_transform<-max(gregexpr(pattern ='_log_',fore_var)[[1]])
  pwr_transform<-max(gregexpr(pattern ='_pwr_',fore_var)[[1]])
  transform_val<-max(gregexpr(pattern ='_',fore_var)[[1]])
  div_pos_orig<-max(gregexpr(pattern ="/",fore_var)[[1]])
  
  if(div_pos_orig>0)
  {
    numerator_orig<-as.numeric(substr(fore_var,transform_val+1,
                                      div_pos_orig-1))
    
    denominator_orig<-as.numeric(substr(fore_var,div_pos_orig+1,
                                        nchar(fore_var)))
  }
  
  if(log_transform>0)
  {
    if(div_pos_orig>0)
    {
      df$y<-ifelse(df$y==0,1,df$y)
      df$y<-log(df$y,base = numerator_orig/denominator_orig)
    }
    else if(substr(fore_var,transform_val+1,
                   nchar(fore_var))=="e")
    {
      df$y<-ifelse(df$y==0,1,df$y)
      df$y<-log(df$y)
    }
    else
    {
      df$y<-ifelse(df$y==0,1,df$y)
      df$y<-log(df$y,base = as.numeric(substr(fore_var,transform_val+1,
                                   nchar(fore_var))))
    }
    
  }
  if(pwr_transform>0)
  {
    if(div_pos_orig>0)
    {
      df$y<-df$y^(numerator_orig/denominator_orig)
    }
    else
    {
      df$y<-df$y^(as.numeric(substr(fore_var,transform_val+1,nchar(fore_var))))
    }
  }
  return(df)
}

fc_agg_df <- function(input,df,orig_df)
{
  
  if(input$tabs=="fb_prophet")
  {
    fore_var<-input$fore_var
    time_var<-input$time_var
    agg_type<-input$agg_type
  }
  else
  {
    fore_var<-input$fore_var_Ari
    time_var<-input$time_var_Ari
    agg_type<-input$agg_type_ari
  }

  log_transform<-max(gregexpr(pattern ='_log_',fore_var)[[1]])
  pwr_transform<-max(gregexpr(pattern ='_pwr_',fore_var)[[1]])
  
  if(log_transform>0)
  {
    orig_df<-orig_df[,match(c(time_var,substr(fore_var,1,log_transform-1)),
                       colnames(orig_df))]
    colnames(orig_df)<-c("ds","y")
    
  }
  else if(pwr_transform>0)
  {
    orig_df<-orig_df[,match(c(time_var,substr(fore_var,1,pwr_transform-1)),
                       colnames(orig_df))]
    colnames(orig_df)<-c("ds","y")
  }
  else
  {
    orig_df<-orig_df[,c(time_var,fore_var)]
    colnames(orig_df)<-c("ds","y")
  }
  
  df<-df[,c(time_var,fore_var)]
  colnames(df)<-c("ds","y")
  
  df$y<-orig_df$y
  
  if(agg_type=="12-hour")
    df<-df
  else if(agg_type=="Daily")
  {
    df$Dates<-as.Date(df$ds)
    df<-aggregate(y~Dates, df, sum)
    colnames(df)[1]<-"ds"
  }
  else if(agg_type=="Weekdays")
  {
    df$Dates<-as.Date(df$ds)
    df<-aggregate(y~Dates, df, sum)
    df$Day<-weekdays(df$Dates)
    df <- df[!df$Day %in% c("Sunday", "Saturday"),c("Dates","y")]
    colnames(df)[1]<-"ds"
  }
  else if(agg_type=="12-hour (Weekdays)")
  {
    df$Dates<-strptime(df$ds,format = "%Y-%m-%d %H:%M:%S",tz="America/New_York")
    df$Day<-weekdays(df$Dates)
    df <- df[!df$Day %in% c("Sunday", "Saturday"),c("ds","y")]
  }
  else if(agg_type=="Weekly (Weekdays)")
  {
    df$Dates<-as.Date(df$ds)
    df<-aggregate(y~Dates, df, sum)
    df$Day<-weekdays(df$Dates)
    df <- df[!df$Day %in% c("Sunday", "Saturday"),]
    df <- df[match("Monday",df$Day):nrow(df),]
    df <- df[1:(nrow(df)-as.POSIXlt(max(df$Dates))$wday%%5),]
    df$week_num <- rep(1:(nrow(df)/5),each=5)
    df_temp1<-aggregate(y~week_num,df,sum)
    df_temp2 <- aggregate(df$Dates,by=list(df$week_num),max)
    df <- merge(df_temp2,df_temp1,by.y ="week_num",by.x ="Group.1")[,-1]
    colnames(df)[1]<-"ds"
  }
  
  return(df)
}

# Function to create working data frame
fc_work_df <- function(input,work_df)
{
  if(input$tabs=="fb_prophet")
  {
    dateRange<-input$dateRange
  }
  else
  {
    dateRange<-input$dateRange_ari
  }
  
  work_df<-work_df[work_df$ds>=dateRange[1],]
  work_df<-work_df[work_df$ds<=dateRange[2],]
  return(work_df)
}

# Function to create holiday data frame
fc_holiday_df <- function()
{
  df<-list()
  df[[1]]<-as.data.frame(fread("holidays.csv"))
  df[[1]]$ds<-as.Date(df[[1]]$ds,"%Y-%m-%d")
  df[[2]]<-as.data.frame(fread("holidays_ari.csv"))
  df[[2]]$ds<-as.Date(df[[2]]$ds)
  df[[3]]<-as.data.frame(fread("holidays_week.csv"))
  df[[3]]$ds<-as.Date(df[[3]]$ds,"%Y-%m-%d")
  df[[4]]<-as.data.frame(fread("holidays_ari_week.csv"))
  df[[4]]$ds<-as.Date(df[[4]]$ds)
  df[[5]]<-as.data.frame(fread("holidays_am_pm.csv"))
  df[[5]]$ds<-as.POSIXct(strptime(df[[5]]$ds,format = "%Y-%m-%d %H:%M:%S",tz="America/New_York"),tz="America/New_York")
  df[[6]]<-as.data.frame(fread("holidays_ari.csv"))
  df[[6]]$ds<-strptime(df[[6]]$ds,format = "%Y-%m-%d %H:%M:%S",tz="America/New_York")
  return(df)   
}

fc_hol_df<-function(input,df_list)
  {
    if(is.null(df))
       return(NULL)
    else
      {
        if(input$tabs=="fb_prophet")
        {
          agg_type<-input$agg_type
          if(agg_type=="Weekly (Weekdays)")
          {
            df<-df_list[[3]]
          }
          else if(agg_type=="12-hour (Weekdays)")
          {
            df<-df_list[[5]]
          }
          else
          {
            df<-df_list[[1]]
          }
          work_df<-df[which(df$holiday %in% input$holiday),]
        }
        else
        {
          agg_type<-input$agg_type_ari
          if(agg_type=="12-hour")
          {
            df<-df_list[[2]]
            work_df<-as.data.frame(df[,which(colnames(df) %in% c("ds",input$holiday.ari))])
            if(ncol(work_df)==1)
              colnames(work_df)<-"ds"
          }
          else if(agg_type=="Daily")
          {
            df<-df_list[[2]]
            temp_df<-as.data.frame(df[,which(colnames(df) %in% c("ds",input$holiday.ari))])
            if(ncol(temp_df)==1)
              colnames(temp_df)<-"ds"
            temp_df$ds<-as.Date(df$ds)
            work_df<-as.data.frame(unique(temp_df))
            if(ncol(work_df)==1)
              colnames(work_df)<-"ds"
          }
          else if(agg_type=="Weekdays")
          {
            df<-df_list[[2]]
            temp_df<-as.data.frame(df[,which(colnames(df) %in% c("ds",input$holiday.ari))])
            if(ncol(temp_df)==1)
              colnames(temp_df)<-"ds"
            temp_df$ds<-as.Date(df$ds)
            temp_df$Day<-weekdays(temp_df$ds)
            temp_df <- temp_df[!temp_df$Day %in% c("Sunday", "Saturday"),]
            work_df<-as.data.frame(unique(temp_df)[,-match(c("Day"),colnames(temp_df))])
            if(ncol(work_df)==1)
              colnames(work_df)<-"ds"
          }
          else if(agg_type=="12-hour (Weekdays)")
          {
            df<-df_list[[6]]
            temp_df<-as.data.frame(df[,which(colnames(df) %in% c("ds",input$holiday.ari))])
            if(ncol(temp_df)==1)
              colnames(temp_df)<-"ds"
            temp_df$ds<-strptime(df$ds,format = "%Y-%m-%d %H:%M:%S",tz="America/New_York")
            temp_df$Day<-weekdays(temp_df$ds)
            work_df <- as.data.frame(temp_df[!temp_df$Day %in% c("Sunday", "Saturday"),-match(c("Day"),colnames(temp_df))])
            if(ncol(work_df)==1)
              colnames(work_df)<-"ds"
          }
          else if(agg_type=="Weekly (Weekdays)")
          {
            df<-df_list[[4]]
            work_df<-as.data.frame(df[,which(colnames(df) %in% c("ds",input$holiday.ari))])
            if(ncol(work_df)==1)
              colnames(work_df)<-"ds"
          }
        }
        
        if(is.null(work_df))
        {
          return(NULL)
        }
        else if(nrow(work_df)!=0)
        {
          return(work_df)
        }
        else
        {
          return(NULL)
        }
      }
}

# Function to create list of seasonality models
seasonality_df <- function(input,season_df){

  if(input$tabs=="fb_prophet")
  {
    season<-input$season
    season.period<-input$season.period
    season.fourier<-input$season.fourier
  }
  else
  {
    season<-input$season.ari
    season.period<-input$season.period.ari
    season.fourier<-input$season.fourier.ari
  }
  
  temp_df<-data.frame(Seasonality=as.character(season),
                      Period=round(as.numeric(season.period),0),
                      Fourier_Order=round(as.numeric(season.fourier),0),
                      stringsAsFactors = FALSE)
  
  season_df<-rbind.data.frame(season_df,temp_df)
  return(season_df)
}


# Function to develop FBProphet Model
fc_model <- function(input,work_df,hol_df,season_df){
  
  
  # Assigning Auto seasonality
  if(is.null(season_df))
  {

    daily_season<-"auto"
    weekly_season<-"auto"
    yearly_season<-"auto"
    
    fbp <- prophet(interval.width = as.numeric(input$interval),
                   holidays = hol_df,
                   daily.seasonality = daily_season,
                   weekly.seasonality = weekly_season,
                   # monthly.seasonality = monthly_season,
                   # quarterly.seasonality = quarterly_season,
                   yearly.seasonality = yearly_season,
                   changepoints = NULL,
                   n.changepoints = as.numeric(input$n_changepoints),
                   seasonality.prior.scale = as.numeric(input$season_scale),
                   holidays.prior.scale = as.numeric(input$holiday_scale),
                   changepoint.prior.scale = as.numeric(input$changepoints_scale),
                   mcmc.samples = round(as.numeric(input$mcmc_samples),0),
                   uncertainty.samples = round(as.numeric(input$uncertainty_samples),0),
                   fit = TRUE)
    
    sink(file = "model.txt",append = FALSE)
    cat("work_df<-read.csv('training_data.csv',stringsAsFactors=FALSE)",fill = TRUE,sep = "")
    cat("hol_df<-read.csv('holidays_used.csv',stringsAsFactors=FALSE)",fill = TRUE,sep = "")
    cat("fbp <- prophet(interval.width =",as.numeric(input$interval),",",fill = TRUE,sep = "")
    cat("               holidays = hol_df,",fill = TRUE,sep = "")
    cat("               daily.seasonality =",daily_season,",",fill = TRUE,sep = "")
    cat("               weekly.seasonality =",weekly_season,",",fill = TRUE,sep = "")
    cat("               yearly.seasonality =",yearly_season,",",fill = TRUE,sep = "")
    cat("               changepoints = NULL,",fill = TRUE,sep = "")
    cat("               n.changepoints =",as.numeric(input$n_changepoints),",",fill = TRUE,sep = "")
    cat("               seasonality.prior.scale =",as.numeric(input$season_scale),",",fill = TRUE,sep = "")
    cat("               holidays.prior.scale =",as.numeric(input$holiday_scale),",",fill = TRUE,sep = "")
    cat("               changepoint.prior.scale =",as.numeric(input$changepoints_scale),",",fill = TRUE,sep = "")
    cat("               mcmc.samples =",round(as.numeric(input$mcmc_samples),0),",",fill = TRUE,sep = "")
    cat("               uncertainty.samples =",round(as.numeric(input$uncertainty_samples),0),",",fill = TRUE,sep = "")
    cat("               fit = TRUE)",fill = TRUE,sep = "")
    sink()
  }
  else
  {
    
    daily_season<-FALSE
    weekly_season<-FALSE
    yearly_season<-FALSE
  
    fbp <- prophet(interval.width = as.numeric(input$interval),
                   holidays = hol_df,
                   daily.seasonality = daily_season,
                   weekly.seasonality = weekly_season,
                   # monthly.seasonality = monthly_season,
                   # quarterly.seasonality = quarterly_season,
                   yearly.seasonality = yearly_season,
                   changepoints = NULL,
                   n.changepoints = as.numeric(input$n_changepoints),
                   seasonality.prior.scale = as.numeric(input$season_scale),
                   holidays.prior.scale = as.numeric(input$holiday_scale),
                   changepoint.prior.scale = as.numeric(input$changepoints_scale),
                   mcmc.samples = round(as.numeric(input$mcmc_samples),0),
                   uncertainty.samples = round(as.numeric(input$uncertainty_samples),0),
                   fit = TRUE)
    
    sink(file = "model.txt",append = FALSE)
    cat("work_df<-read.csv('training_data.csv',stringsAsFactors=FALSE)",fill = TRUE,sep = "")
    cat("hol_df<-read.csv('holidays_used.csv',stringsAsFactors=FALSE)",fill = TRUE,sep = "")
    cat("fbp <- prophet(interval.width =",as.numeric(input$interval),",",fill = TRUE,sep = "")
    cat("               holidays = hol_df,",fill = TRUE,sep = "")
    cat("               daily.seasonality =",daily_season,",",fill = TRUE,sep = "")
    cat("               weekly.seasonality =",weekly_season,",",fill = TRUE,sep = "")
    cat("               yearly.seasonality =",yearly_season,",",fill = TRUE,sep = "")
    cat("               changepoints = NULL,",fill = TRUE,sep = "")
    cat("               n.changepoints =",as.numeric(input$n_changepoints),",",fill = TRUE,sep = "")
    cat("               seasonality.prior.scale =",as.numeric(input$season_scale),",",fill = TRUE,sep = "")
    cat("               holidays.prior.scale =",as.numeric(input$holiday_scale),",",fill = TRUE,sep = "")
    cat("               changepoint.prior.scale =",as.numeric(input$changepoints_scale),",",fill = TRUE,sep = "")
    cat("               mcmc.samples =",round(as.numeric(input$mcmc_samples),0),",",fill = TRUE,sep = "")
    cat("               uncertainty.samples =",round(as.numeric(input$uncertainty_samples),0),",",fill = TRUE,sep = "")
    cat("               fit = TRUE)",fill = TRUE,sep = "")
    sink()
    
    for(i in 1:nrow(season_df))
    {
      fbp<-add_seasonality(m=fbp,name=season_df$Seasonality[i],
                           period = season_df$Period[i],
                           fourier.order = season_df$Fourier_Order[i])
      sink(file = "model.txt",append = TRUE)
      cat("fbp<-add_seasonality(m=fbp,name='",season_df$Seasonality[i],"',",fill = TRUE,sep = "")
      cat("                           period =",season_df$Period[i],",",fill = TRUE,sep = "")
      cat("                           fourier.order =",season_df$Fourier_Order[i],")",fill = TRUE,sep = "")
      sink()
    }
  }
  
  fbp<-fit.prophet(df=work_df,m=fbp)
  
  write.csv(work_df,"training_data.csv",row.names = FALSE)
  write.csv(hol_df,"holidays_used.csv",row.names = FALSE)
  sink(file = "model.txt",append = TRUE)
  cat("fbp<-fit.prophet(df=work_df,m=fbp)",fill = TRUE)
  sink()
  
  return(fbp)
}

# Function to develop ARIMA Model
fc_model_ar <- function(input,work_df,xreg){

  if(is.null(xreg))
  {
    tryCatch(
      {
        fbp <- Arima(y = work_df$y, order = c(
          as.numeric(input$p_ari),as.numeric(input$d_ari),as.numeric(input$q_ari)))
        sink(file = "model.txt",append = FALSE)
        cat("work_df<-read.csv('training_data.csv',stringsAsFactors=FALSE)",fill = TRUE,sep = "")
        cat("fbp <- Arima(y = work_df$y, order = c(",as.numeric(input$p_ari),",",as.numeric(input$d_ari),",",as.numeric(input$q_ari),"))",fill = TRUE,sep = "")
        sink()
        write.csv(work_df,"training_data.csv",row.names = FALSE)
        write.csv(xreg,"arimax_regressors.csv",row.names = FALSE)
        return(fbp)
      },
      error=function(cond) {
        # Choose a return value in case of error
        return(NULL)
      },
      warning=function(cond) {
        # Choose a return value in case of warning
        return(NULL)
      })
  }
  else if(ncol(xreg)==0)
  {
    
    tryCatch(
      {
        fbp <- Arima(y = work_df$y, order = c(
          as.numeric(input$p_ari),as.numeric(input$d_ari),as.numeric(input$q_ari)))
        sink(file = "model.txt",append = FALSE)
        cat("work_df<-read.csv('training_data.csv',stringsAsFactors=FALSE)",fill = TRUE,sep = "")
        cat("fbp <- Arima(y = work_df$y, order = c(",as.numeric(input$p_ari),",",as.numeric(input$d_ari),",",as.numeric(input$q_ari),"))",fill = TRUE,sep = "")
        sink()
        write.csv(work_df,"training_data.csv",row.names = FALSE)
        write.csv(xreg,"arimax_regressors.csv",row.names = FALSE)
        return(fbp)
      },
      error=function(cond) {
        # Choose a return value in case of error
        return(NULL)
      },
      warning=function(cond) {
        # Choose a return value in case of warning
        return(NULL)
      })
    
  }
  else
  {
    tryCatch(
    {
      fbp <- Arima(y = work_df$y, xreg = xreg[1:nrow(work_df),], order = c(
        as.numeric(input$p_ari),as.numeric(input$d_ari),as.numeric(input$q_ari)))
      
      sink(file = "model.txt",append = FALSE)
      cat("work_df<-read.csv('training_data.csv',stringsAsFactors=FALSE)",fill = TRUE,sep = "")
      cat("xreg<-read.csv('arimax_regressors.csv',stringsAsFactors=FALSE)",fill = TRUE,sep = "")
      cat("fbp <- Arima(y = work_df$y,xreg = xreg[1:nrow(work_df),],
          order = c(",as.numeric(input$p_ari),",",as.numeric(input$d_ari),",",as.numeric(input$q_ari),"))",fill = TRUE,sep = "")
      sink()
      write.csv(work_df,"training_data.csv",row.names = FALSE)
      write.csv(xreg,"arimax_regressors.csv",row.names = FALSE)
      return(fbp)
    },
    error=function(cond) {
    # Choose a return value in case of error
      return(NULL)
    },
    warning=function(cond) {
    # Choose a return value in case of warning
      return(NULL)
    })
  }
}

arima_regressors<-function(df,season_df,hol_df)
{
  if(is.null(season_df))
  {
    if(!is.null(hol_df))
    {
      temp_df<-as.data.frame(df$ds)
      names(temp_df)<-"ds"
      hol_df<-merge(temp_df,hol_df,by="ds")
      xreg<-hol_df
      xreg<-as.data.frame(xreg[,-match("ds",colnames(xreg))])
      names(xreg)<-colnames(hol_df)[-match("ds",colnames(hol_df))]
    }
    else
      xreg<-NULL
  }
  else
  {
    y<-msts(df$y, seasonal.periods = as.numeric(season_df$Period))
    regressor <- fourier(y, as.numeric(season_df$Fourier_Order))
    if(!is.null(hol_df))
    {
      temp_df<-as.data.frame(df$ds)
      names(temp_df)<-"ds"
      hol_df<-merge(temp_df,hol_df,by="ds")
      xreg<-cbind(regressor, hol_df)
      xreg<-xreg[,-match("ds",colnames(xreg))]
    }
    else
      xreg<-regressor
  }
  return(xreg)
}

time_frame_prediction<-function(input,work_df,full_df)
{
  if(input$tabs=="fb_prophet")
  {
    dateRange<-input$dateRange
    time_period<-input$time_period
    agg_type<-input$agg_type
  }
  else
  {
    dateRange<-input$dateRange_ari
    time_period<-input$time_period_ari
    agg_type<-input$agg_type_ari
  }
  
  temp_df<-full_df[full_df$ds>=dateRange[1],]
  total_rows<-nrow(work_df)+time_period
  if(total_rows<=nrow(temp_df))
  {
    temp_df<-temp_df[1:total_rows,]
  }
  else if(agg_type=="12-hour")
  {
    temp_df<-data.frame(ds=c(temp_df$ds,seq(max(temp_df$ds),
                                            by=(as.POSIXct("2014-12-31 23:59:59",
                                                           "%Y-%m-%d %H:%M:%S",
                                                           tz="America/New_York")-
                                                  as.POSIXct("2014-12-31 11:59:59",
                                                             "%Y-%m-%d %H:%M:%S",
                                                             tz="America/New_York")),
                                            length.out = (total_rows-nrow(temp_df)+1))[-1]),
                        stringsAsFactors = FALSE)
  }
  else if(agg_type=="Daily")
  {
    temp_df<-data.frame(ds=c(temp_df$ds,
                             seq(max(temp_df$ds),
                                 by="day",
                                 length.out=(total_rows-nrow(temp_df)+1))[-1]),
                        stringsAsFactors = FALSE)
  }
  else if(agg_type=="Weekdays")
  {
    temp_df_t<-data.frame(ds=c(temp_df$ds,
                               seq(max(temp_df$ds),
                                   by="day",
                                   length.out=(total_rows-nrow(temp_df)+1+
                                                 floor(total_rows-
                                                         nrow(temp_df))*2))[-1]),
                          stringsAsFactors = FALSE)
    
    temp_df_t$Day<-weekdays(temp_df_t$ds)
    temp_df <- data.frame(temp_df_t[!temp_df_t$Day %in% c("Sunday", "Saturday"),"ds"])
  }
  else if(agg_type=="12-hour (Weekdays)")
  {
    temp_df_t<-data.frame(ds=c(temp_df$ds,seq(max(temp_df$ds),
                                              by=(as.POSIXct("2014-12-31 23:59:59",
                                                             "%Y-%m-%d %H:%M:%S",
                                                             tz="America/New_York")-
                                                    as.POSIXct("2014-12-31 11:59:59",
                                                               "%Y-%m-%d %H:%M:%S",
                                                               tz="America/New_York")),
                                              length.out = (total_rows-nrow(temp_df)+1+
                                                              floor(total_rows-
                                                                      nrow(temp_df))*2))[-1]),
                          stringsAsFactors = FALSE)
    
    temp_df_t$Day<-weekdays(temp_df_t$ds)
    temp_df <- data.frame(temp_df_t[!temp_df_t$Day %in% c("Sunday", "Saturday"),"ds"])
  }
  else
  {
    temp_df <-data.frame(ds=c(temp_df$ds,
                              seq(max(temp_df$ds),
                                  by="week",
                                  length.out = (total_rows-nrow(temp_df)+1)[-1])),
                         stringsAsFactors = FALSE)
    
  }
  return(temp_df)
}

# Function to predict using created model
predicted_model<-function(input,model_fb,work_df,full_df)
{
  fbp<-model_fb
  temp_df<-time_frame_prediction(input,work_df,full_df)
  future <- data.frame(ds=temp_df$ds,stringsAsFactors = FALSE)
  write.csv(future,"data_to_predict.csv",row.names = FALSE)
  Predicted<-predict(fbp, future)
  sink(file = "model.txt",append = TRUE)
  cat("future<-read.csv('data_to_predict.csv',stringsAsFactors = FALSE)",fill = TRUE)
  cat("Predicted<-predict(fbp, future)")
  sink()
  return(Predicted)
}


# Function to predict using created model
predicted_model_ar<-function(input,model_ar,work_df,xreg)
{
  if(is.null(xreg))
  {
    Predicted <- forecast(model_ar, h = input$time_period_ari)
    sink(file = "model.txt",append = TRUE)
    cat("Predicted<-forecast(fbp, h =",input$time_period_ari, ")",fill = TRUE,sep = "")
    sink()
    return(Predicted)
  }
  
  else if(ncol(xreg)==0)
  {
    Predicted <- forecast(model_ar, h = input$time_period_ari)
    sink(file = "model.txt",append = TRUE)
    cat("Predicted<-forecast(fbp, h =",input$time_period_ari, ")",fill = TRUE,sep = "")
    sink()
    return(Predicted)
  }
  else
  {
    tryCatch(
    {
      if(input$time_period_ari==1)
      {
        # xreg_temp<-data.frame(t(xreg[(nrow(work_df)+1):(nrow(work_df)+input$time_period_ari),]))
        xreg_temp<-data.frame((xreg[(nrow(work_df)+1):(nrow(work_df)+input$time_period_ari),]))
        
      }
      else
        xreg_temp<-xreg[(nrow(work_df)+1):(nrow(work_df)+input$time_period_ari),]
      Predicted <- forecast(model_ar, h = input$time_period_ari,
                           xreg=xreg_temp)
      sink(file = "model.txt",append = TRUE)
      cat("Predicted<-forecast(fbp, h =",input$time_period_ari,
        ",xreg=xreg[",(nrow(work_df)+1),":",(nrow(work_df)+input$time_period_ari),",])",fill = TRUE,sep = "")
      sink()
      return(Predicted)
    },
    error=function(cond) {
      # Choose a return value in case of error
      return(NULL)
    },
    warning=function(cond) {
      # Choose a return value in case of warning
      return(NULL)
    })
  }
}


pro_forecast_df<-function(input,train_df,work_df,orig_df,pred_model){
  
  tryCatch(
    {
      if(input$tabs=="fb_prophet")
      {
        showgrid<-input$showgrid
      }
      
      if(input$tabs=="arimax")
      {
        showgrid<-input$showgrid_ari
      }
      
      # predicted <- reactive({
        
        if(input$tabs=="fb_prophet")
        {
          fore_var<-input$fore_var
          dateRange<-input$dateRange
          showgrid<-input$showgrid
          
          Predicted<-pred_model
          predicted.fit<-Predicted$yhat
          predicted.lwr<-Predicted$yhat_lower
          predicted.upr<-Predicted$yhat_upper
          dates<-Predicted$ds
        }
        
        if(input$tabs=="arimax")
        {
          fore_var<-input$fore_var_Ari
          dateRange<-input$dateRange_ari
          showgrid<-input$showgrid_ari
          
          predicted.upr <- as.numeric(pred_model$fitted) + 1.96*sqrt(as.numeric(pred_model$model$sigma2))
          predicted.upr <- c(predicted.upr,as.numeric(pred_model$upper[,2]))
          predicted.lwr <- as.numeric(pred_model$fitted) - 1.96*sqrt(as.numeric(pred_model$model$sigma2))
          predicted.lwr <- c(predicted.lwr,as.numeric(pred_model$lower[,2]))
          predicted.fit <- c(as.numeric(pred_model$fitted),as.numeric(pred_model$mean))
          dates<-time_frame_prediction(input,train_df,work_df)$ds
        }
        
        work_df$y<-orig_df$y
        
        work_df<-work_df[work_df$ds>=dateRange[1],]
        
        log_transform<-max(gregexpr(pattern ='_log_',fore_var)[[1]])
        pwr_transform<-max(gregexpr(pattern ='_pwr_',fore_var)[[1]])
        transform_val<-max(gregexpr(pattern ='_',fore_var)[[1]])
        
        div_pos_orig<-max(gregexpr(pattern ="/",fore_var)[[1]])
        if(div_pos_orig>0)
        {
          numerator_orig<-as.numeric(substr(fore_var,transform_val+1,
                                            div_pos_orig-1))
          
          denominator_orig<-as.numeric(substr(fore_var,div_pos_orig+1,
                                              nchar(fore_var)))
        }
        
        if(log_transform>0)
        {
          if(div_pos_orig>0)
          {
            predicted.fit<-(numerator_orig/denominator_orig)^predicted.fit
            predicted.lwr <- (numerator_orig/denominator_orig)^predicted.lwr
            predicted.upr <- (numerator_orig/denominator_orig)^predicted.upr
          }
          else if(substr(fore_var,transform_val+1,
                         nchar(fore_var))=="e")
          {
            predicted.fit<-exp(1)^predicted.fit
            predicted.lwr <- exp(1)^predicted.lwr
            predicted.upr <- exp(1)^predicted.upr
          }
          else
          {
            predicted.fit<-as.numeric(substr(fore_var,transform_val+1,
                                             nchar(fore_var)))^predicted.fit
            predicted.lwr <- as.numeric(substr(fore_var,transform_val+1,
                                                   nchar(fore_var)))^predicted.lwr
            predicted.upr <- as.numeric(substr(fore_var,transform_val+1,
                                               nchar(fore_var)))^predicted.upr
          }
          
        }
        if(pwr_transform>0)
        {
          if(div_pos_orig>0)
          {
            predicted.fit<-predicted.fit^denominator_orig/numerator_orig
            predicted.lwr <- predicted.lwr^denominator_orig/numerator_orig
            predicted.upr <- predicted.upr^denominator_orig/numerator_orig
          }
          else
          {
            predicted.fit<-predicted.fit^(1/as.numeric(substr(fore_var,transform_val+1,nchar(fore_var))))
            predicted.lwr <- predicted.lwr^(1/as.numeric(substr(fore_var,transform_val+1,nchar(fore_var))))
            predicted.upr <- predicted.upr^(1/as.numeric(substr(fore_var,transform_val+1,nchar(fore_var))))
          }
        }
        
        if(length(predicted.fit)>nrow(work_df))
        {
          Actual<-c(work_df$y,rep(NA,(length(predicted.fit)-nrow(work_df))))
        }
        else
          Actual<-work_df$y[1:length(predicted.fit)]
        
        
      # })
      df<-cbind.data.frame(dates,Actual,predicted.fit,predicted.lwr,predicted.upr)
      return(df)
    },
    error=function(cond) {
      # Choose a return value in case of error
      return(NULL)
    },
    warning=function(cond) {
      # Choose a return value in case of warning
      return(NULL)
    })
}


pro_forecast_func<-function(input,df)
{
  if(is.null(df))
    return(NULL)
  else
  {
    if(input$tabs=="fb_prophet")
    {
      showgrid<-input$showgrid
    }
    else if(input$tabs=="arimax")
    {
      showgrid<-input$showgrid_ari
    }
    Dates<-df$dates
    dygraph(xts(df[,2:5],order.by=Dates), main = "Forecast of orders") %>%
    dySeries(paste0("predicted.", c("lwr", "fit", "upr")), label = "Predicted") %>%
    dyRangeSelector(height=20) %>%
    dyOptions(drawGrid = showgrid)
  }
}

convert_to_original<-function(input,x)
{
  
  if(input$tabs=="fb_prophet")
  {
    fore_var<-input$fore_var
  }
  
  else
  {
    fore_var<-input$fore_var_Ari
  }
  
  log_transform<-max(gregexpr(pattern ='_log_',fore_var)[[1]])
  pwr_transform<-max(gregexpr(pattern ='_pwr_',fore_var)[[1]])
  transform_val<-max(gregexpr(pattern ='_',fore_var)[[1]])
  
  div_pos_orig<-max(gregexpr(pattern ="/",fore_var)[[1]])
  if(div_pos_orig>0)
  {
    numerator_orig<-as.numeric(substr(fore_var,transform_val+1,
                                      div_pos_orig-1))
    
    denominator_orig<-as.numeric(substr(fore_var,div_pos_orig+1,
                                        nchar(fore_var)))
  }
  
  if(log_transform>0)
  {
    if(div_pos_orig>0)
    {
      x <- (numerator_orig/denominator_orig)^x
    }
    else if(substr(fore_var,transform_val+1,
                   nchar(fore_var))=="e")
    {
      x <- exp(1)^x
    }
    else
    {
      x <- as.numeric(substr(fore_var,transform_val+1,
                      nchar(fore_var)))^x
    }
    
  }
  if(pwr_transform>0)
  {
    if(div_pos_orig>0)
    {
      x <- x^denominator_orig/numerator_orig
    }
    else
    {
      x <- x^(1/as.numeric(substr(fore_var,transform_val+1,nchar(fore_var))))
    }
  }
  return(x)
}


# Function to calculate accuracy/error
error_table<-function(input,work_df,pred_model,df,orig_df){
  tryCatch(
  {  
    train_rows<-nrow(work_df)
    
    if(input$tabs=="fb_prophet")
    {
      Predicted<-pred_model
      pred_model_values<-Predicted$yhat
      dateRange<-input$dateRange
      time_period<-input$time_period
    }
    if(input$tabs=="arimax")
    {
      pred_model_values<-c(pred_model$fitted,pred_model$mean)
      dateRange<-input$dateRange_ari
      time_period<-input$time_period_ari
    }
    
    work_df_temp<-df
    
    work_df_temp$y<-orig_df$y
    
    work_df_temp<-work_df_temp[work_df_temp$ds>=dateRange[1],]
    
    if((train_rows+time_period)<=nrow(work_df_temp))
    {
      work_df<-work_df_temp[1:(train_rows+time_period),]
    }
    else
    {
      work_df<-work_df_temp
    }
    
    fp <- convert_to_original(input,pred_model_values)
  
    temp_df<-data.frame(ds=work_df[,'ds'],act_y=work_df[,'y'],pred_y=fp,
                        stringsAsFactors = FALSE)
    temp_df_train<-temp_df[1:train_rows,]
    temp_df_test<-temp_df[(train_rows+1):nrow(temp_df),]
    
    df_to_write<-temp_df_train
    df_to_write$Train_Test<-rep("Train",nrow(temp_df_train))
    df_to_write<-rbind.data.frame(df_to_write,
                                  data.frame(Train_Test=rep("Test",nrow(temp_df_test)),
                                             ds=temp_df_test$ds,
                                             act_y=temp_df_test$act_y,
                                             pred_y=temp_df_test$pred_y,
                                             stringsAsFactors = FALSE))
    write.csv(df_to_write,"Predictions.csv",row.names = FALSE)
    
    error_absolute_chart<-data.frame(Date=temp_df_train$ds,
                                     Train_Error=round(abs(temp_df_train$act_y-temp_df_train$pred_y)
                                                       ,2),
                                     Test_Error=NA,
                                     stringsAsFactors = FALSE)
    
    error_absolute_chart<-rbind.data.frame(
      error_absolute_chart,
      data.frame(Date=temp_df_test$ds,
                Train_Error=NA,
                Test_Error=round(abs(temp_df_test$act_y-temp_df_test$pred_y)
                                ,2),
                stringsAsFactors = FALSE))
    
    error_absolute_chart[nrow(temp_df_train),3]<-error_absolute_chart[nrow(temp_df_train),2]
    colnames(error_absolute_chart)<-c("Date","Train AE",
                                     "Test AE")
    
    temp_df_train<-temp_df_train[temp_df_train$act_y!=0,]
    temp_df_test<-temp_df_test[temp_df_test$act_y!=0,]
    
    error_percent_chart<-data.frame(Date=temp_df_train$ds,
                                    Train_Error=round(abs((temp_df_train$act_y-temp_df_train$pred_y)/
                                              temp_df_train$act_y*100),2),
                                    Test_Error=NA,
                                    stringsAsFactors = FALSE)
    
    
    
    MAPE_Train<-round(mean(error_percent_chart$Train_Error),2)
    
    error_percent_chart<-rbind.data.frame(error_percent_chart,
                         data.frame(Date=temp_df_test$ds,
                         Train_Error=NA,
                         Test_Error=round(abs((temp_df_test$act_y-temp_df_test$pred_y)/
                                                temp_df_test$act_y*100),2),
                         stringsAsFactors = FALSE))
    
    MAPE_Test<-round(mean(error_percent_chart$Test_Error,na.rm = TRUE),2)
    
    error_percent_chart[nrow(temp_df_train),3]<-error_percent_chart[nrow(temp_df_train),2]
    colnames(error_percent_chart)<-c("Date","Train Error (%)",
                                     "Test Error (%)")
    
    
    final_df<-data.frame(a=MAPE_Train,b=MAPE_Test,stringsAsFactors = FALSE)
    colnames(final_df)<-c("MAPE Train (%)","MAPE Test(%)")
    
    result<-list()
    result[[1]]<-final_df
    result[[2]]<-error_percent_chart
    result[[3]]<-error_absolute_chart
    return(result)
  },
  error=function(cond) {
    # Choose a return value in case of error
    return(NULL)
  },
  warning=function(cond) {
    # Choose a return value in case of warning
    return(NULL)
  })
}

error_chart<-function(input,result)
{
  if(is.null(result))
    return(NULL)
  else
  {
    if(input$tabs=="fb_prophet")
    {
      error_chart<-input$error_chart
    }
    else if(input$tabs=="arimax")
    {
      error_chart<-input$error_chart_ari
    } 
    if(error_chart=="Percentage Error")
    {
      df<-result[[2]]
      
      dygraph(xts(df,order.by=df$Date),
              main = "Percentage Error") %>%
        dyRangeSelector(height=20)
    }
    else
    {
      df<-result[[3]]
      dygraph(xts(df,order.by=df$Date),
              main = "Absolute Error") %>%
        dyRangeSelector(height=20)
    }
  }
}


computeMASE <- function(actual_train,actual_test,forecast_train,forecast_test,period)
{
  meanMASE<-list()
  period<-as.numeric(period)
  actual_train <- as.vector(actual_train)
  actual_test <- as.vector(actual_test)
  forecast_train<-as.vector(forecast_train)
  forecast_test<-as.vector(forecast_test)
  
  n <- length(actual_train)
  
  full_data<-c(actual_train,actual_test)
  
  naive_forecast<-c(rep(NA,period),full_data[1:(length(full_data)-period)])
  train_scaling<-mean(abs(naive_forecast[1:n]-actual_train),na.rm=TRUE)
  test_scaling<-mean(abs(naive_forecast[(n+1):length(naive_forecast)]-actual_test),na.rm=TRUE)
  
  et <- abs(actual_train-forecast_train)
  qt <- et/train_scaling
  meanMASE$Train <- round(mean(qt),2)
  et <- abs(actual_test-forecast_test)
  qt <- et/test_scaling
  meanMASE$Test <- round(mean(qt),2)
  
  return(meanMASE)
}

#n-period increase validation
n_fold_validation<-function(input,full_df,orig_df,hol_df,season_df)
{
  
  if(input$tabs=="fb_prophet")
  {
    dateRange<-input$dateRange
    cut_off<-input$cut_off
    time_period<-input$time_period
    naive_period<-input$naive_period
    agg_type<-input$agg_type
  }
  
  if(input$tabs=="arimax")
  {
    xreg<-arima_regressors(full_df,season_df,hol_df)
    dateRange<-input$dateRange_ari
    cut_off<-input$cut_off_ari
    time_period<-input$time_period_ari
    naive_period<-input$naive_period_ari
    agg_type<-input$agg_type_ari
  }
  
  cut_off_date<-c()
  Train_MASE<-c()
  Test_MASE<-c()
  Train_MAPE<-c()
  Test_MAPE<-c()
  Train_MAE<-c()
  Test_MAE<-c()
  Test_Data_Values<-c()
  
  orig_train_rows<-nrow(fc_work_df(input,full_df))
  work_df<-full_df[full_df$ds>=dateRange[1],]
  work_orig_df<-orig_df[orig_df$ds>=dateRange[1],]
  
  cross_validation_table<-work_df
  colnames(cross_validation_table)<-c("Date","Actual")
  
  test_range<-nrow(work_df)-orig_train_rows
  i<-1
  while((orig_train_rows+(i-1)*cut_off+time_period)<=nrow(work_df))
  {
    
    train <- work_df[1:(orig_train_rows + ((i-1)*cut_off)),]
    
    test <- work_df[(orig_train_rows+(i-1)*cut_off+1):
                    (orig_train_rows+(i-1)*cut_off+time_period),]
    
    temp_df<-rbind.data.frame(train,test)
    
    if(input$tabs=="fb_prophet")
    {
      temp_model<-fc_model(input,train,hol_df,season_df)
      predict_df<-data.frame(ds=temp_df$ds,stringsAsFactors = FALSE)
      temp_predicted_model<-predict(temp_model,predict_df)
      
      fp <- temp_predicted_model[,c('yhat')]
    }
    
    if(input$tabs=="arimax")
    {
      
      if(ncol(xreg)==0)
      {
        temp_model<-fc_model_ar(input,train,xreg)
        temp_predicted_model <- forecast(temp_model, h = input$time_period_ari)
      }
      else
      {
        temp_model<-fc_model_ar(input,train,xreg)
        temp_predicted_model <- forecast(temp_model, h = input$time_period_ari,
                              xreg=xreg[(nrow(train)+1):(nrow(train)+input$time_period_ari),])
      }
      
      fp <- c(temp_model$fitted,temp_predicted_model$mean)
    }
    
    train<-work_orig_df[1:(orig_train_rows + ((i-1)*cut_off)),]
    
    test<-work_orig_df[(orig_train_rows+(i-1)*cut_off+1):
                       (orig_train_rows+(i-1)*cut_off+time_period),]
    
    fp<-convert_to_original(input,fp)
    
    train$y_hat<-fp[1:nrow(train)]
    test$y_hat<-fp[(nrow(train)+1):length(fp)]
    
    MASE_Value<-computeMASE(train$y,test$y,train$y_hat,test$y_hat,naive_period)
    temp_MASE_train <- MASE_Value[[1]]
    temp_MASE_test <- MASE_Value[[2]]
    
    test_data_values_temp<-c(test$y,test$y_hat)
    
    temp_train_MAE<-round(mean(abs(train$y-train$y_hat)),2)
    temp_test_MAE<-round(mean(abs(test$y-test$y_hat)),2)
    
    cut_off_date<-c(cut_off_date,train$ds[nrow(train)])
    
    fit_df<-rbind.data.frame(train,test)
    
    cross_validation_table$Forecast<-c(fit_df$y_hat,
                                       rep(NA,(nrow(cross_validation_table)-nrow(fit_df))))
    colnames(cross_validation_table)[ncol(cross_validation_table)]<-paste0("Forecast_",i)
    write.csv(cross_validation_table,"CV_data.csv",row.names = FALSE)
    
    train<-train[train$y!=0,]
    test<-test[test$y!=0,]
    
    temp_train_MAPE<-round(mean(abs((train$y-train$y_hat))/train$y*100),2)
    temp_test_MAPE<-round(mean(abs((test$y-test$y_hat))/test$y*100),2)
    
    
    Train_MASE<-c(Train_MASE,temp_MASE_train)
    Test_MASE<-c(Test_MASE,temp_MASE_test)
    Train_MAPE<-c(Train_MAPE,temp_train_MAPE)
    Test_MAPE<-c(Test_MAPE,temp_test_MAPE)
    Train_MAE<-c(Train_MAE,temp_train_MAE)
    Test_MAE<-c(Test_MAE,temp_test_MAE)
    Test_Data_Values<-rbind.data.frame(Test_Data_Values,test_data_values_temp)
    
    
    i<-i+1
  }
  
  colnames(Test_Data_Values)[1:(ncol(Test_Data_Values)/2)]<-paste0("Actual_",1:(ncol(Test_Data_Values)/2))
  colnames(Test_Data_Values)[(ncol(Test_Data_Values)/2+1):ncol(Test_Data_Values)]<-paste0(
    "Predicted_",1:(ncol(Test_Data_Values)/2))
  
  k_fold_table<-data.frame(cut_off_date,
                           Train_MAE,
                           Test_MAE,
                           Train_MAPE,
                           Test_MAPE,stringsAsFactors = FALSE)
  
  k_fold_table_output<-data.frame(cut_off_date,
                           Train_MAE,
                           Test_MAE,
                           Train_MAPE,
                           Test_MAPE,
                           Train_MASE,
                           Test_MASE,stringsAsFactors = FALSE)
  
  k_fold_table_output<-cbind.data.frame(k_fold_table_output,Test_Data_Values)
  
  
  if(agg_type=="12-hour")
  {
    k_fold_table$cut_off_date<-as.POSIXlt(k_fold_table$cut_off_date,origin = "1970-01-01",
                                          tz = "America/New_York")
    k_fold_table_output$cut_off_date<-as.POSIXlt(k_fold_table_output$cut_off_date,
                                                 origin = "1970-01-01",
                                          tz = "America/New_York")
  }
  else if(agg_type=="Day-Level")
  {
    k_fold_table$cut_off_date<-as.Date(k_fold_table$cut_off_date)
    k_fold_table_output$cut_off_date<-as.Date(k_fold_table_output$cut_off_date)
  }
  else if(agg_type=="Weekdays")
  {
    k_fold_table$cut_off_date<-as.Date(k_fold_table$cut_off_date)
    k_fold_table_output$cut_off_date<-as.Date(k_fold_table_output$cut_off_date)
  }
  else if(agg_type=="Weekly (Weekdays)")
  {
    k_fold_table$cut_off_date<-as.Date(k_fold_table$cut_off_date)
    k_fold_table_output$cut_off_date<-as.Date(k_fold_table_output$cut_off_date)
  }
  else
  {
    k_fold_table$cut_off_date<-as.Date(k_fold_table$cut_off_date)
    k_fold_table_output$cut_off_date<-as.Date(k_fold_table_output$cut_off_date)
    # k_fold_table$cut_off_date<-as.POSIXlt(k_fold_table$cut_off_date,origin = "1970-01-01",
    #                                       tz = "America/New_York")
    # k_fold_table_output$cut_off_date<-as.POSIXlt(k_fold_table_output$cut_off_date,
    #                                              origin = "1970-01-01",
    #                                              tz = "America/New_York")
  }
  colnames(k_fold_table)<-c("Training Cut-off Date","Train MAE",
                            "Test MAE","Train MAPE (%)","Test MAPE (%)")
  colnames(k_fold_table_output)[1:7]<-c("Training Cut-off Date","Train MAE",
                            "Test MAE","Train MAPE (%)","Test MAPE (%)","Train MASE",
                            "Test MASE")
  write.csv(k_fold_table_output,"k_fold_table.csv",row.names = FALSE)
  return(k_fold_table)
}