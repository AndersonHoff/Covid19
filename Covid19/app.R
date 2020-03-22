#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
if(!require(dplyr)) {istall.packages("dplyr")} else {library(dplyr)}
if(!require(ggplot2)) {install.packages("ggplot2")} else {library(ggplot2)}
library(RJSONIO)
library(ggplot2)
library(dplyr)
library(magrittr)
#library(kableExtra)

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
   titlePanel("COVID-19 Countries"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("countryInput", "Country",
                     choices = Countries, selected = "")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("graph"),
        br(), br(),
        tableOutput("results")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  filtered <- reactive({dataset[dataset$Country==input$countryInput, ]})
  
    output$results <- renderTable({
#      filtered[order(-filtered$Confirmed, -filtered$Days),]
#      filtered <- filter(dataset, Country == input$countryInput)
#      knitr::kable(filtered()[1:10,2:5])
      filtered()
    })
    
    output$graph <- renderPlot({
      ggplot(filtered(), aes(x=Days, y=Confirmed))+
        geom_bar(stat = "identity")+
        theme_bw()+
        ylab("Confirmed Cases")+
        xlab("Days")
    
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

