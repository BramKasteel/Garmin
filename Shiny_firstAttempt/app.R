library(shiny)
library(shinyjs)
library(plotly)
library(reshape2)


source('CreateSQLConnection.R')
all_cons <- dbListConnections(MySQL())
for(con in all_cons) dbDisconnect(con)
db <- CreateSQLConnection()

activity <- dbGetQuery(db,"SELECT id FROM garmin.activity")
vars <- dbGetQuery(db,"SELECT DISTINCT(variable) FROM garmin.record")

ui <- fluidPage(
  # Based on k-means example from shiny.rstudio.com
  shinyjs::useShinyjs(),
  div(id = 'form',
  pageWithSidebar(
    headerPanel("Bram's runs"),
    sidebarPanel(
      selectInput('type', 'Choose to display single or multiple activities',
                  c('single','longRuns','intervalTraining','races','classify')),
      uiOutput('chosenType')
    )
    ,
    
    
    mainPanel(
      plotlyOutput('plot1'),
      uiOutput('secondPlot')
    )
  )
  )
)

server <- function(input,output){
  #Only update when told so
  observeEvent(input$go, {
    print(as.numeric(input$go))
  })
  
  observeEvent(input$goClassify, {
    print(as.numeric(input$goClassify))
    dbGetQuery(db,paste0("UPDATE garmin.activity SET traintype = '",input$trainType,"' WHERE id = '",input$actId,"'"))
    shinyjs::reset("form")
  })
  observeEvent(input$restart,{
    shinyjs::reset("form")
  })
  
  output$chosenType <- renderUI({
    if (input$type == 'single'){
      output$columns <- renderUI({
        available <- dbGetQuery(db, paste0("SELECT * FROM garmin.activity WHERE created = '", input$actId,"' LIMIT 1"))
        fluidPage(
          selectInput('xVar', 'Select X var', names(available)[available==1]),
          selectInput('yVar', 'Select Y var', names(available)[available==1]),
          actionButton(inputId = 'go', label = 'Update')
        )
      })
      created <- dbGetQuery(db, "SELECT created FROM garmin.activity")
      created <- as.POSIXct(created[,1],tz="UTC")
      created <- sort(created,decreasing=T)
      created <- format(created,tz="UTC")
      bootstrapPage(
        selectInput('actId', 'Choose activity', created),
        uiOutput('columns')
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
  
  #Render the data
  data <- eventReactive(input$go, {
    if (input$type!='classify'){
      actId <- dbGetQuery(db,paste0("SELECT id FROM garmin.activity WHERE created='",input$actId,"'"))[1,1]
      query <- paste0("SELECT * FROM garmin.record WHERE id='",actId,"' AND variable IN ('",input$xVar,"', '",input$yVar,"')")
      dataSelected <- dbGetQuery(db,query)
      dataSelected <- dcast(dataSelected, timestamp ~ variable ,value.var = 'value')
    } else if (input$type=='classify'){
      query <- paste0("SELECT * FROM garmin.record WHERE id='",input$actId,"' AND variable IN ('",input$xVar,"', '",input$yVar,"')")
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
  
  plotTmp <- eventReactive(input$go, {
    dataHere <- data()
    p <- plot_ly(data = dataHere, x=dataHere[,names(dataHere)%in%input$xVar], y=dataHere[,names(dataHere)%in%input$yVar],mode='markers',type='scatter')
  })
  
  output$plot1 <- renderPlotly({
    plotTmp()
  })
  
  plotTime <- eventReactive(input$go, {
    dataHere <- data()
    ay <- list(
      #tickfont = list(color = "red"),
      overlaying = "y",
      side = "right",
      title = "y-axis2"
    )
    p <- plot_ly(data=dataHere, x=dataHere$timestamp, y = dataHere[,names(dataHere)%in%input$xVar],mode='markers',type='scatter',name=input$xVar) %>%
      add_trace(data=dataHere, x=dataHere$timestamp, y=dataHere[,names(dataHere)%in%input$yVar],mode='markers',type='scatter',name=input$yVar,yaxis='y2')%>%
      layout(title='Both versus time', yaxis2=ay, xaxis=list(title='time'))
  })
  
  output$plot2 <- renderPlotly({
      plotTime()
  })
  

  output$secondPlot <- renderUI({
    if (input$type == 'single'){
      fluidPage({
        plotlyOutput('plot2')
      })
    } else {
      fluidPage({})
    }
  })
}

shinyApp(ui=ui,server=server)
