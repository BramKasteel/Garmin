library(fit)

path <- "FIT/"

filenames <- list.files(path)
filenames <- paste0(path, filenames)

data <- lapply(filenames, read.fit)

#Test
namesData <- names(data[[1]])
session <- data[[length(data)]]
units <- attr(session$record, 'units')
record <- session$record

View(record)
#pre processing
record_NA <- record
record_NA$timestamp <- record_NA$timestamp - record_NA$timestamp[1]
record_NA[record_NA$cadence < 0.9*median(record_NA$cadence),]<- NA
library(plotly)
ay <- list(tickfont=list(color = 'black'),overlaying='y',
           side='right',title='speed (m/s)')
plot_ly(data=record_NA,x=record_NA$timestamp,y=record_NA$heart_rate,name='heart rate')%>%
  add_trace(data=record_NA,x=record_NA$timestamp,y=record_NA$speed,yaxis='y2',name='speed') %>%
  layout(title = 'hr and speed',yaxis2=ay,xaxis=list(title='time'))

plot_ly(data=record_NA,x=record_NA$distance,y=record_NA$heart_rate,name='heart rate')%>%
  add_trace(data=record_NA,x=record_NA$distance,y=record_NA$speed,yaxis='y2',name='speed') %>%
  layout(title = 'hr and speed',yaxis2=ay,xaxis=list(title='dist'))

#Smoothed
library(caTools)
record_NA$heart_rate <- smooth(record_NA$heart_rate)
record_NA$cadence <- runmean(record_NA$cadence,k=5,align='center',endrule='keep',alg='fast')
record_NA$speed <- runmean(record_NA$speed,k=5,align='center',endrule='keep',alg='fast')

ay <- list(tickfont=list(color = 'black'),overlaying='y',
           side='right',title='speed (m/s)')
plot_ly(data=record_NA,x=record_NA$timestamp,y=record_NA$heart_rate,name='heart rate')%>%
  add_trace(data=record_NA,x=record_NA$timestamp,y=record_NA$speed,yaxis='y2',name='speed') %>%
  layout(title = 'hr and speed',yaxis2=ay,xaxis=list(title='time'))

plot_ly(data=record_NA,x=record_NA$distance,y=record_NA$heart_rate,name='heart rate')%>%
  add_trace(data=record_NA,x=record_NA$distance,y=record_NA$speed,yaxis='y2',name='speed') %>%
  layout(title = 'hr and speed',yaxis2=ay,xaxis=list(title='dist'))
