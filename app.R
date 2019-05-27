library(shiny)
library(leaflet)
library(dplyr)
df<-data.frame(read.csv("WorldcitiesNsites4.csv"))
#df<-data.frame(read.csv("C:/Users/asala/Coursera/Developing Data Products/ShinyTest/Shinier/WorldcitiesNsites4.csv"))
      df$UsMel1DateIA<-as.Date(df$UsMel1DateIA) 	
      df$UsMel1DateDBL<-as.Date(df$UsMel1DateDBL) 	
      df$UsMel2DateIA<-as.Date(df$UsMel2DateIA) 	
      df$UsMel2DateDBL<-as.Date(df$UsMel2DateDBL) 	
      df$UsMel3DateIA<-as.Date(df$UsMel3DateIA) 	
      df$UsMel3DateDBL<-as.Date(df$UsMel3DateDBL) 	
      df$UsPanc1DateIA<-as.Date(df$UsPanc1DateIA) 	
      df$UsPanc1DateDBL<-as.Date(df$UsPanc1DateDBL) 	
      df$UsPanc2DateIA<-as.Date(df$UsPanc2DateIA) 	
      df$UsPanc2DateDBL<-as.Date(df$UsPanc2DateDBL) 	
ui <- fluidPage(
   titlePanel("Trial Sites Comparison"),
   sidebarLayout(
      sidebarPanel(
            selectInput("choice1","Who?",choices=c("Us","Them","Both")),
            selectInput("choice2","Which?",choices=c("Mel1","Mel2","Mel3","Panc1","Panc2")),
            selectInput("choice3","Interim Analysis (IA) or Database Lock (DBL)",choices=c("IA","DBL")),
            dateInput("dateS", "Starting Date (for selected activity):", value = "2018-06-27", format = "mm/dd/yy"),
            dateInput("dateE", "Ending Date (for selected activity):", value = "2020-06-27", format = "mm/dd/yy")
      ),
      mainPanel(
            leafletOutput("siteMap")
      )
   )
)
server <- function(input, output) {
      ptext1<-"  Site details 1, 2, 3"
      ptext2<-"  Site details a, b, c"
      output$siteMap <- renderLeaflet({
            DateUsT<-paste("Us",input$choice2,"Date",input$choice3,sep="")
            df$InRange<-NULL
            df$InRange<-FALSE
            for(i in 1:nrow(df)) {
                  if(df[DateUsT][[1]][i] >= input$dateS & df[DateUsT][[1]][i] <= input$dateE){
                        df$InRange[i]<-TRUE
                  }
            }
            comboUs<-paste("Us",input$choice2,sep="")
            comboUsOnly<-paste("UsOnly",input$choice2,sep="")
            comboThem<-paste("Them",input$choice2,sep="")
            comboThemOnly<-paste("ThemOnly",input$choice2,sep="")
            comboBoth<-paste("Both",input$choice2,sep="")
            dfUs<-dplyr::filter(df,df[comboUs][[1]])
            dfUs<-dplyr::filter(dfUs,dfUs$InRange)
            #dfUs<-dfUs[dfUs[DateUsT][[1]] >= input$dateS & dfUs[DateUsT][[1]] <= input$dateE,]
            dfUsOnly<-dplyr::filter(df,df[comboUsOnly][[1]])
            dfUsOnly<-dplyr::filter(dfUsOnly,dfUsOnly$InRange)
            #dfUsOnly<-dfUsOnly[dfUsOnly[DateUsT][[1]] >= input$dateS & dfUsOnly[DateUsT][[1]] <= input$dateE,]
            dfThem<-dplyr::filter(df,df[comboThem][[1]])
            dfThemOnly<-dplyr::filter(df,df[comboThemOnly][[1]])
            dfBoth<-dplyr::filter(df,df[comboBoth][[1]])
            dfBoth<-dplyr::filter(dfBoth,dfBoth$InRange)
            #dfBoth<-dfBoth[dfBoth[DateUsT][[1]] >= input$dateS & dfBoth[DateUsT][[1]] <= input$dateE,]
            dfAll<-dplyr::filter(df,df[comboUs][[1]]==TRUE|df[comboThem][[1]]==TRUE)
            if(input$choice1=="Us") {
                  df %>% 
                        leaflet() %>% 
                        addTiles() %>%
                        addCircleMarkers(lat=dfUs$lat,lng=dfUs$lng,radius=5,color="blue") %>%
                        addMarkers(lat=dfUs$lat,lng=dfUs$lng,popup=paste(dfUs$city, "<br>",ptext1, "<br>",ptext2),clusterOptions = markerClusterOptions())
            }
            else if(input$choice1=="Them") {
                  df %>% 
                        leaflet() %>% 
                        addTiles() %>%
                        addCircleMarkers(lat=dfThem$lat,lng=dfThem$lng,radius=3,color="red") %>%
                        addMarkers(lat=dfThem$lat,lng=dfThem$lng,popup=paste(dfThem$city, "<br>",ptext1, "<br>",ptext2),clusterOptions = markerClusterOptions())
            }
            else if(input$choice1=="Both") {
                  df %>% 
                        leaflet() %>% 
                        addTiles() %>%
                        addCircleMarkers(lat=dfUsOnly$lat,lng=dfUsOnly$lng,radius=5,color="blue") %>%
                        addCircleMarkers(lat=dfThemOnly$lat,lng=dfThemOnly$lng,radius=3,color="red") %>%
                        addCircleMarkers(lat=dfBoth$lat,lng=dfBoth$lng,radius=7,color="purple") %>%
                        addMarkers(lat=dfAll$lat,lng=dfAll$lng,popup=paste(dfAll$city, "<br>",ptext1, "<br>",ptext2),clusterOptions = markerClusterOptions())
            }
      })
}
shinyApp(ui = ui, server = server)

