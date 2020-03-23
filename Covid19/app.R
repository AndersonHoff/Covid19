#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(RJSONIO)
library(magrittr)
library(kableExtra)

url <- "https://pomber.github.io/covid19/timeseries.json"
destfile <- "timeseries.json"
download.file(url, destfile, mode="wb")

datajson <- fromJSON('timeseries.json')

data <- reshape2::melt(datajson)[, c("L1", "L2", "L3", "value")]

dataset <- reshape2::dcast(data, L1+L2~L3)

colnames(dataset) <- c("Country", "Days", "Confirmed", "Date","Deaths", "Recovered" )

#dataset$Date <- as.Date(dataset$Date, format = "%Y-%m-%d")

dataset$Confirmed <- as.integer(dataset$Confirmed)

dataset$Deaths <- as.integer(dataset$Deaths)

dataset$Recovered <- as.integer(dataset$Recovered)

#dataset$Country <- as.factor(dataset$Country) ### see if factor or character!!!!

Countries <- as.factor(unique(dataset$Country))

dataset <- dataset[order(dataset$Country, -dataset$Confirmed, -dataset$Days),]

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("COVID-19 PROBLEM"),
   p("Diagrams illustrating the increase number of cases of COVID-19 in different countries"),
   tabsetPanel(
     tabPanel("Diagrams")
   ),
   
   sidebarLayout(
      sidebarPanel(
         selectInput("countryInput", "Country",
                     choices = Countries, selected = "")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("graph"),
        br(), br(),
        plotOutput("deaths"),
        br(),
        plotOutput("recover"),
        br(),
        tableOutput("results")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  filtered <- reactive({dataset[dataset$Country==input$countryInput, ]})
  
    output$results <- renderTable({
#      kable(filtered)
      filtered()
    })
    
    output$graph <- renderPlot({
      ggplot(filtered(), aes(x=Days, y=Confirmed))+
        geom_bar(stat = "identity", fill = "darkblue")+
        theme_bw(base_size = 25) +
        ggtitle("CONFIRMED CASES OF COVID-19")+
        ylab("Confirmed Cases")+
        xlab("Days")
    
   })
    
    output$deaths <- renderPlot({
      ggplot(filtered(), aes(x=Days, y=Deaths))+
        geom_bar(stat = "identity", fill = "darkblue")+
        theme_bw(base_size = 25) +
        ggtitle("DEATHS FROM COVID-19")+
        ylab("Deaths Cases")+
        xlab("Days")
      
    })
    
    output$recover <- renderPlot({
      ggplot(filtered(), aes(x=Days, y=Recovered))+
        geom_bar(stat = "identity", fill = "darkblue")+
        theme_bw(base_size = 25) +
        ggtitle("RECOVERED FROM COVID-19")+
        ylab("Recovered Cases")+
        xlab("Days")
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

