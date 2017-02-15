library(shiny)
library(shinyjs)
library(plotly)
library(reshape2)


source('CreateSQLConnection.R')
all_cons <- dbListConnections(MySQL())
for(con in all_cons) dbDisconnect(con)
db <- CreateSQLConnection()

activity <- dbGetQuery(db,"SELECT id FROM garmin.activity")
vars <- dbGetQuery(db,"SELECT * FROM garmin.activity LIMIT 1")
vars <- setdiff(names(vars),c('id','user','created','trainType'))

ui <- fluidPage(
  shinyjs::useShinyjs(),
  div(id = 'form',
      pageWithSidebar(
        headerPanel("Bram's runs"),
        sidebarPanel(
          selectInput('type', 'Choose to display single or multiple activities',
                      c('single','longRuns','intervalTraining','races','classify')),
          uiOutput('chosenType'),
          ###DEZE MOET STRAKS IN EEN APARTE NESTED SIDEBAR OFZO: mooier :)
          div(id = 'inputs',
              uiOutput('plotInputs'))
        )
        ,
        mainPanel(
          div(id = 'plotWindow',
          plotlyOutput('plot1'))
        )
        ## RESETTING DOES NOT WORK
      )
  )
)

server <- function(input,output){

#Events based on buttons
  
  observeEvent(input$goClassify, {
    dbGetQuery(db,paste0("UPDATE garmin.activity SET traintype = '",input$trainType,"' WHERE id = '",input$actId,"'"))
    shinyjs::reset("form")
  })
  
  observeEvent(input$type,{
    shinyjs::reset('inputs')
    shinyjs::reset('plotWindow')
    #shinyjs::reset('form')
  })
  
  observeEvent(input$restart,{
    shinyjs::reset("form")
  })

# PART ONE: SELECT ONE OR MULTIPLE ACTIVITIES
  output$columnsId <- renderUI({
    available <- dbGetQuery(db, paste0("SELECT * FROM garmin.activity WHERE created = '", input$actId,"' LIMIT 1"))
    actionButton(inputId = 'go', label = 'Go to plot')
  })
  
  createdF <- reactive({
    created <- dbGetQuery(db, paste0("SELECT created FROM garmin.activity WHERE trainType IN ('",paste(input$trainType,collapse="','"),"')"))
    created <- as.POSIXct(created[,1],tz="UTC")
    created <- sort(created,decreasing=T)
    created <- format(created,tz="UTC")
    return(created)
  })
  
  output$singleAct <- renderUI({
    tagList(
    selectInput('actId', 'Choose activity', createdF()),
    actionButton(inputId = 'go', label = 'Select variables')
    )
  })
  
  output$chosenType <- renderUI({
    if (input$type == 'single'){
      tagList(
        checkboxGroupInput("trainType", label = NULL, 
                           choices = list("Interval training" = 'intervalTraining', "Long runs" = 'longRuns'),
                           selected = c('intervalTraining')),
        uiOutput('singleAct')
      )
    } else if (input$type == 'classify'){
      unclassified <- dbGetQuery(db, "SELECT id FROM garmin.activity WHERE trainType IS NULL")
      if (nrow(unclassified)>0){
        output$columns <- renderUI({
          available <- dbGetQuery(db, paste0("SELECT * FROM garmin.activity WHERE id = '", input$actId,"' LIMIT 1"))
          fluidPage(
            selectInput('xVar', 'Select X var', names(available)[available==1], selected='timestamp'),
            selectInput('yVar', 'Select Y var', names(available)[available==1], selected='heart_rate'),
            actionButton(inputId = 'go', label = 'Update'),
            selectInput('trainType', 'Classify this training',
                        c('longRuns','intervalTraining','races','unknown')),
            actionButton(inputId = 'goClassify', label = 'Classify')
          )
        })
        bootstrapPage(
          selectInput('actId', 'Choose activity', unclassified[,1]),
          uiOutput('columns')
        )
      } else {
        output$columns <- renderUI({
          actionButton(inputId = 'restart',label='Nothing to classify, restart')
        })
        bootstrapPage(
          uiOutput('columns')  
        )
      }
    } else {
      bootstrapPage(
        selectInput('xVar', 'X Variable', vars),
        selectInput('yVar', 'Y Variable', vars),
        actionButton(inputId = 'go', label = 'Update')
      )
    }
  })
  
## PART TWO: PLOTTING
  #Render the data
  data <- eventReactive(input$go, {
    if (input$type=='single'){
      actId <- dbGetQuery(db,paste0("SELECT id FROM garmin.activity WHERE created='",input$actId,"'"))[1,1]
      query <- paste0("SELECT * FROM garmin.record WHERE id='",actId,"'")
      dataSelected <- dbGetQuery(db,query)
      dataSelected <- dcast(dataSelected, timestamp ~ variable ,value.var = 'value')
    } else if (input$type=='classify'){
      actId <- dbGetQuery(db,paste0("SELECT id FROM garmin.activity WHERE created='",input$actId,"'"))[1,1]
      query <- paste0("SELECT * FROM garmin.record WHERE id='",actId,"' AND variable IN ('",input$xVar,"', '",input$yVar,"')")
      dataSelected <- dbGetQuery(db,query)
      dataSelected <- dcast(dataSelected, timestamp ~ variable ,value.var = 'value')
    } else {
      query <- paste0("SELECT id FROM garmin.activity WHERE trainType='",input$type,"'")
      trainIds <- dbGetQuery(db,query)
      dataSelected <- data.frame(x=1,y=2)
      names(dataSelected) <- c(input$xVar,input$yVar)
      dataSelected <- dataSelected[-1,]
      for (id in trainIds[,1]){
        query <- paste0("SELECT * FROM garmin.record WHERE id='",id,"' AND variable IN ('",input$xVar,"', '",input$yVar,"')")
        dataTmp <- dbGetQuery(db,query)
        dataTmp <- dcast(dataTmp, timestamp ~ variable ,value.var = 'value')
        dataSelected <- rbind(dataSelected,dataTmp[,c(2,3)])
      }
    }
    return(dataSelected)
  })
  
  output$plotInputs <- renderUI({
    dataHere <- data()
    choices <- setNames(colnames(dataHere),colnames(dataHere))
    tagList(
      br(),
      selectInput('xVar', 'Select X var', colnames(dataHere), selected='timestamp'),
      checkboxGroupInput("yVar", label = 'Select Y vars', choices = choices),
      actionButton(inputId = 'plot', label = 'Plot')
    )
  })
  
  plotTmp <- eventReactive(input$plot, {
    data <- data()
    
    #TODO https://plot.ly/r/setting-graph-size/
    
    if (length(input$yVar)>0){
      p <- plot_ly(data = data, x=as.formula(paste0("~",input$xVar)), y=as.formula(paste0("~",input$yVar[1])),mode='markers',type='scatter',name=input$yVar[1])
      p <- layout(p,title=paste0("Plot of (",paste(input$yVar,collapse=','),") against ",input$xVar))
    }
    if (length(input$yVar)>1){
      trace = 2
      while(trace <= length(input$yVar)){
        p <- add_trace(p,y=data[[input$yVar[trace]]],yaxis=paste0('y',trace),name=input$yVar[trace])
        ay <- list(p=p,list(overlaying='y',side='right',title=input$yVar[trace],position=1-0.066*(trace-2)))
        p <- do.call(layout,setNames(ay,c('p',paste0('yaxis',trace))))
        trace = trace+1
      }
    }
    return(p)
  })
  
  output$plot1 <- renderPlotly({
    p <- plot_ly(data = data.frame(x=c(1),y=c(1)),x=~x,y=~y, type='scatter',mode='markers') %>% layout(title='Nothing to plot yet')
    
    if (length(input$yVar)>0){
      p <- plotTmp()
    }
    p
  })
  
  
  
  
}

shinyApp(ui=ui,server=server)



# output$columns <- renderUI({
#   available <- dbGetQuery(db, paste0("SELECT * FROM garmin.activity WHERE created = '", input$actId,"' LIMIT 1"))
#   fluidPage(
#     selectInput('xVar', 'Select X var', names(available)[available==1]),
#     selectInput('yVar', 'Select Y var', names(available)[available==1]),
#     actionButton(inputId = 'go', label = 'Update')
#   )
# })
# created <- dbGetQuery(db, "SELECT created FROM garmin.activity")
# created <- as.POSIXct(created[,1],tz="UTC")
# created <- sort(created,decreasing=T)
# created <- format(created,tz="UTC")
# bootstrapPage(
#   selectInput('actId', 'Choose activity', created),
#   uiOutput('columns')
# )

# plotTime <- eventReactive(input$go, {
#   dataHere <- data()
#   ay <- list(
#     #tickfont = list(color = "red"),
#     overlaying = "y",
#     side = "right",
#     title = "y-axis2"
#   )
#   p <- plot_ly(data=dataHere, x=dataHere$timestamp, y = dataHere[,names(dataHere)%in%input$xVar],mode='markers',type='scatter',name=input$xVar) %>%
#     add_trace(data=dataHere, x=dataHere$timestamp, y=dataHere[,names(dataHere)%in%input$yVar],mode='markers',type='scatter',name=input$yVar,yaxis='y2')%>%
#     layout(title='Both versus time', yaxis2=ay, xaxis=list(title='time'))
# })

# output$plot2 <- renderPlotly({
#   plotTime()
# })
# 
# 
# output$secondPlot <- renderUI({
#   if (input$type == 'single'){
#     fluidPage({
#       plotlyOutput('plot2')
#     })
#   } else {
#     fluidPage({})
#   }
# })