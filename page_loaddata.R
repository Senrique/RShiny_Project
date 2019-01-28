page.LoadData.UI<-renderUI({fluidRow(
  # Sidebar with a slider input for the number of bins
  column(3,
         wellPanel(class="sidebar",
           selectInput("fileType", "Select File Type:",
                       c("Text/CSV (.csv)" = "csv")
           ),
           fileInput('file1', 'Choose file to upload',
                     accept = c(
                       'text/csv',
                       'text/comma-separated-values',
                       'text/tab-separated-values',
                       'text/plain',
                       '.csv',
                       '.tsv'
                     )
           ),
           tags$hr(),
           checkboxInput('header', 'Header', TRUE),
           radioButtons('sep', 'Separator',
                        c(Comma=',',
                          Semicolon=';',
                          Tab='\t'),
                        ','),
           radioButtons('quote', 'Quote',
                        c(None='',
                          'Double Quote'='"',
                          'Single Quote'="'"),
                        '"'),
           tags$hr(),
           uiOutput("export_file")
         )),
  
  # Show a plot of the generated distribution
  column(8,offset=1,
         tabsetPanel(
           tabPanel("Data Frame Structure",
                    dataTableOutput('table_str')),
           tabPanel("Data View",
             fluidRow(
               column(width=3,
                      uiOutput("LD.var")),
               column(width=3,
                      uiOutput("LD.datatype")
                      ),
               column(width=3,
                      uiOutput("LD.format")),
               column(width=3,
                      uiOutput("LD.cust.format"))),
             fluidRow(
               column(width=3,
                      uiOutput("LD.update.table"))),
             hr(),
             fluidRow("",
                    dataTableOutput('table')))
           ))
)})


page_LD.UI.var<-function (df){
  ui<-renderUI(selectInput("pg_LD.var","Variables",
                           choices=c("Select",unique(colnames(df))),
                           selected="Select")) 
  return(ui)
}

page_LD.UI.datatype<-function (input){
  ui<-renderUI(selectInput("pg_LD.DT","Change to Datatype or Transform the variable to",
                           if(input$pg_LD.var=='Select')
                           "Not Applicable"
                           else
                           {c("Date","Character","Integer","Numeric","Factor",
                              "Log","Power")})) 
  return(ui)
}

page_LD.UI.format<-function (input){
  ui<-renderUI(selectInput("pg_LD.format","Select Current Format",
                           if(input$pg_LD.DT=='Date')
                           {c("Custom","mm/dd/yy (12/31/01)","mm/dd/yyyy (12/31/2001)",
                              "dd/mm/yy (31/12/01)","dd/mm/yyyy (31/12/2001)",
                              "mm-dd-yy (12-31-01)","mm-dd-yyyy (12-31-2001)",
                              "dd-mm-yy (31-12-01)","dd-mm-yyyy (31-12-2001)",
                              "yy/mm/dd (01/12/31)","yyyy/mm/dd (2001/12/31)",
                              "yy-mm-dd (01-12-31)","yyyy-mm-dd (2001-12-31)",
                              "mm/dd/yy hh:mm:ss (12/31/01 05:25:05)",
                              "mm/dd/yyyy hh:mm:ss (12/31/2001 05:25:05)",
                              "dd/mm/yy hh:mm:ss (31/12/01 05:25:05)",
                              "dd/mm/yyyy hh:mm:ss (31/12/2001 05:25:05)",
                              "mm-dd-yy hh:mm:ss (12-31-01 05:25:05)",
                              "mm-dd-yyyy hh:mm:ss (12-31-2001 05:25:05)",
                              "dd-mm-yy hh:mm:ss (31-12-01 05:25:05)",
                              "dd-mm-yyyy hh:mm:ss (31-12-2001 05:25:05)",
                              "yy/mm/dd hh:mm:ss (01/12/31 05:25:05)",
                              "yyyy/mm/dd hh:mm:ss (2001/12/31 05:25:05)",
                              "yy-mm-dd hh:mm:ss (01-12-31 05:25:05)",
                              "yyyy-mm-dd hh:mm:ss (2001-12-31 05:25:05)")}
                           else if(any(c('Log','Power')==input$pg_LD.DT))
                           {c("Character","Numeric","Factor")}
                           else
                             "Not Applicable")) 
  return(ui)
}

page_LD.UI.format_TB<-function (input){
  ui<-renderUI(textInput("pg_LD.form.tb", label = "R Format input", value = 
                           if(input$pg_LD.format=="mm/dd/yy (12/31/01)")
                           {"%m/%d/%y"}
  else if(input$pg_LD.format=="mm/dd/yyyy (12/31/2001)")
  {"%m/%d/%Y"}
  else if(input$pg_LD.format=="dd/mm/yy (31/12/01)")
  {"%d/%m/%y"}
  else if(input$pg_LD.format=="dd/mm/yyyy (12/31/2001)")
  {"%d/%m/%Y"}
  else if(input$pg_LD.format=="mm-dd-yy (12-31-01)")
  {"%m-%d-%y"}
  else if(input$pg_LD.format=="mm-dd-yyyy (12-31-2001)")
  {"%m-%d-%Y"}
  else if(input$pg_LD.format=="yy/mm/dd (01/12/31)")
  {"%y/%m/%d"}
  else if(input$pg_LD.format=="yyyy/mm/dd (2001/12/31)")
  {"%Y/%m/%d"}
  else if(input$pg_LD.format=="yy-mm-dd (01-12-31)")
  {"%y-%m-%d"}
  else if(input$pg_LD.format=="yyyy-mm-dd (2001-12-31)")
  {"%Y-%m-%d"}
  else if(input$pg_LD.format=="mm/dd/yy hh:mm:ss (12/31/01 05:25:05)")
  {"%m/%d/%y %H:%M:%S"}
  else if(input$pg_LD.format=="mm/dd/yyyy hh:mm:ss (12/31/2001 05:25:05)")
  {"%m/%d/%Y %H:%M:%S"}
  else if(input$pg_LD.format=="dd/mm/yy hh:mm:ss (31/12/01 05:25:05)")
  {"%d/%m/%y %H:%M:%S"}
  else if(input$pg_LD.format=="dd/mm/yyyy hh:mm:ss (31/12/2001 05:25:05)")
  {"%d/%m/%Y %H:%M:%S"}
  else if(input$pg_LD.format=="mm-dd-yy hh:mm:ss (12-31-01 05:25:05)")
  {"%m-%d-%y %H:%M:%S"}
  else if(input$pg_LD.format=="mm-dd-yyyy hh:mm:ss (12-31-2001 05:25:05)")
  {"%m-%d-%Y %H:%M:%S"}
  else if(input$pg_LD.format=="dd-mm-yy hh:mm:ss (31-12-01 05:25:05)")
  {"%d-%m-%y %H:%M:%S"}
  else if(input$pg_LD.format=="dd-mm-yyyy hh:mm:ss (31-12-2001 05:25:05)")
  {"%d-%m-%Y %H:%M:%S"}
  else if(input$pg_LD.format=="yy/mm/dd hh:mm:ss (01/12/31 05:25:05)")
  {"%y/%m/%d %H:%M:%S"}
  else if(input$pg_LD.format=="yyyy/mm/dd hh:mm:ss (2001/12/31 05:25:05)")
  {"%Y/%m/%d %H:%M:%S"}
  else if(input$pg_LD.format=="yy-mm-dd hh:mm:ss (01-12-31 05:25:05)")
  {"%y-%m-%d %H:%M:%S"}
  else if(input$pg_LD.format=="yyyy-mm-dd hh:mm:ss (2001-12-31 05:25:05)")
  {"%Y-%m-%d %H:%M:%S"}
  else if(input$pg_LD.DT=='Log')
  {"e"}
  else if(input$pg_LD.DT=='Power')
  {"1"}
  else
    ""))
  return(ui)
}

pg.update_table<-function(){
  ui<-renderUI(actionButton("pg.btndt.update",
                            label="Update"))
  return(ui)
}

btnAdd_col<-function(df){
  ui<-renderUI(actionButton("Add",
                            label="Add Variable"))
  return(ui)
}

pg.loadData.outputDataType<-function(input,output,df){
  if (is.null(df))
    return(NULL)
  
  ui<-renderUI({
    varNames<-names(df)
    max_pred <- 100 # default 5000
    lapply(1:length(varNames), function(i) {
      col= "#999999"
      ui_div<-paste0("padding-top:1px;background-color:",col)
      
      fluidRow(
        column(width=6,div(style = ui_div),
               checkboxInput(inputId=paste0("pg.loadData.varSel.",
                                            varNames[i]),
                             label=varNames[i],value=TRUE)),
        column(width=6,div(style = ui_div),
               selectInput(inputId=paste0("pg.loadData.varType.",
                                          varNames[i]),
                           label="",
                           c('logical',
                             'double','integer',
                             'character','date',
                             'complex'),
                           typeof(df[,varNames[i]]))))
    })
  })
  return(ui)
  
}

pg.loadData.export_file<-function(){
  ui<-renderUI(actionButton("pg.btnExportFile",
                            label="Export data (.csv)"))
  return(ui)
}


