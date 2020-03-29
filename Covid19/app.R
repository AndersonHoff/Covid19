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
#library(RJSONIO)
library(magrittr)
library(reshape2)
library(jsonlite)

###############################
url <- "https://pomber.github.io/covid19/timeseries.json"
destfile <- "timeseries.json"
download.file(url, destfile, mode="wb")

datajson <- jsonlite::fromJSON('timeseries.json')

#data <- reshape2::melt(datajson)

dataset <- reshape2::melt(datajson, id.vars = c("date", "confirmed","deaths","recovered"))

#dataset <- reshape2::dcast(data, date+~L3)

colnames(dataset) <- c("Date","Confirmed",  "Deaths", "Recovered","Country")

dataset$Confirmed <- as.integer(dataset$Confirmed)

dataset$Deaths <- as.integer(dataset$Deaths)

dataset$Recovered <- as.integer(dataset$Recovered)

Countries <- as.factor(unique(dataset$Country))

#dataset <- dataset[order(dataset$Country, -dataset$Confirmed, -dataset$Date),]

dataset <- dataset[order(dataset$Country, -dataset$Confirmed),]

dataset$Date <- as.Date(dataset$Date, format = "%Y-%m-%d")

coeff = 10
################################################################
#dataset$Date2 <- format(dataset$Date, "%d-%m-%Y")

#Tot_confirmed <-tapply(dataset$Confirmed, dataset$Date, FUN=sum)
total_confirmed <- aggregate(dataset$Confirmed, by=list(Category=dataset$Date), FUN=sum)
colnames(total_confirmed) <- c("Date","Confirmed")

total_death <- aggregate(dataset$Deaths, by=list(Category=dataset$Date), FUN=sum)
colnames(total_death) <- c("Date","Deaths")
##################################################################
population <- read.csv('WPopulation.csv', stringsAsFactors = F, header = TRUE)

max_confirmed <- aggregate(dataset$Confirmed, by = list(dataset$Country), max)

colnames(max_confirmed) <- c("Country", "Confirmed")

max_country <- merge(population, max_confirmed, by="Country" )

max_country$Percentual <- (max_country$Confirmed/max_country$Population)*100

max_country <- max_country %>%
  mutate(rank=rank(-Percentual), 
         Value_rel=Percentual/Percentual[rank==1],
         Value_lbl = paste0("", Percentual)) %>%
  filter(rank<=20)

max_country <- max_country[order(-max_country$rank),]

#####################

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   headerPanel("COVID-19 PROBLEM"),
   p("Diagrams illustrating the increase number of cases of COVID-19 in different countries."),
   
   plotOutput("TOTAL"),
   plotOutput("Percentual"),
   
   p("Data for each Country"),
   
    sidebarPanel(
       selectInput("countryInput", "Country",
                   choices = Countries, selected = "")
    ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("Contamined", plotOutput("graph")),
          tabPanel("Deaths", plotOutput("deaths")),
          tabPanel("EvolutionTable", dataTableOutput("results"))
          
      )
 ))


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  filtered <- reactive({dataset[dataset$Country==input$countryInput, ]})
  
    output$results <- renderDataTable({
      filtered() 
    })
    
    output$TOTAL <- renderPlot({
      ggplot()+
        geom_area(mapping = aes(x=total_confirmed$Date, y=total_confirmed$Confirmed), stat = "identity", fill = "darkblue")+
        geom_area(mapping = aes(x=total_death$Date, y=total_death$Deaths*coeff), fill = "red")+
        theme_bw(base_size = 22) +
        scale_y_continuous(name="Confirmed Cases", labels = scales::comma,
                           sec.axis = sec_axis(~ . * 0.1, name = "Deaths",labels = scales::comma))+
        ggtitle("CONFIRMED CASES OF COVID-19")+
        xlab("")+
        theme(
          axis.title.y = element_text(color = "blue"),
          axis.ticks.y = element_line(color = "blue"),
          axis.line.y = element_line(color = "blue"),
          axis.text.y = element_text(color = "blue"),
          axis.title.y.right = element_text(color = "red"),
          axis.ticks.y.right = element_line(color = "red"),
          axis.line.y.right = element_line(color = "red"),
          axis.text.y.right = element_text(color = "red"))
      
    })
    
    output$Percentual <- renderPlot({
      ggplot(max_country, aes(x=Country, y=Percentual))+
        geom_bar(stat = "identity", fill = "darkblue")+
        theme_bw(base_size = 17) +
        ggtitle("% OF CONFIRMED CASES X TOTAL POPULATION (20 highest rates)")+
        ylab("% of total population")+
        xlab("Country")+
        coord_flip()
    })
    
    output$graph <- renderPlot({
      ggplot(filtered(), aes(x=Date, y=Confirmed))+
        geom_bar(stat = "identity", fill = "darkblue")+
        theme_bw(base_size = 25) +
        ggtitle("CONFIRMED CASES OF COVID-19")+
        ylab("Confirmed Cases")+
        xlab("")
    
   })
    
    output$deaths <- renderPlot({
      ggplot(filtered(), aes(x=Date, y=Deaths))+
        geom_bar(stat = "identity", fill = "darkblue")+
        theme_bw(base_size = 25) +
        ggtitle("DEATHS FROM COVID-19")+
        ylab("Deaths Cases")+
        xlab("")
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

