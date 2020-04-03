############### App of COVID-19 evolution ######################

library(shiny)
library(dplyr)
library(ggplot2)
library(magrittr)
library(reshape2)
library(jsonlite)

###############################
url <- "https://pomber.github.io/covid19/timeseries.json"
destfile <- "timeseries.json"
download.file(url, destfile,method = "curl", mode="wb")

datajson <- jsonlite::fromJSON('timeseries.json')

dataset <- reshape2::melt(datajson, id.vars = c("date", "confirmed","deaths","recovered"))

colnames(dataset) <- c("Date","Confirmed",  "Deaths", "Recovered","Country")

dataset$Confirmed <- as.integer(dataset$Confirmed)

dataset$Deaths <- as.integer(dataset$Deaths)

dataset$Recovered <- as.integer(dataset$Recovered)

Countries <- as.factor(unique(dataset$Country))

dataset <- dataset[order(dataset$Country, -dataset$Confirmed),]

dataset$Date <- as.Date(dataset$Date, format = "%Y-%m-%d")

coeff = 10
################################################################

eachday <- dataset[order(dataset$Country, dataset$Confirmed),]
eachday <- eachday %>%
  select(-Recovered) %>%
  group_by(Country) %>%
  mutate(diary = Confirmed - lag(Confirmed, default = 0))

###############################################################

total_confirmed <- aggregate(dataset$Confirmed, by=list(Category=dataset$Date), FUN=sum)
colnames(total_confirmed) <- c("Date","Confirmed")

total_death <- aggregate(dataset$Deaths, by=list(Category=dataset$Date), FUN=sum)
colnames(total_death) <- c("Date","Deaths")

day_confirmed <- aggregate(eachday$diary, by=list(Category=eachday$Date), FUN=sum)
colnames(day_confirmed) <- c("Date","Day_Confirmed ")

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

max_country <- max_country[order(max_country$rank),]

#############################

library(rworldmap)

#create a map-shaped window
mapDevice('x11')
#join to a coarse resolution map
spdf <- joinCountryData2Map(max_confirmed, joinCode="NAME", nameJoinColumn="Country")

mapCountryData(spdf, nameColumnToPlot="Confirmed", numCats = 16,catMethod="fixedWidth", 
               colourPalette="diverging")

savePlot(filename=paste0("www/map.png"),type="png")
dev.off()

########################################################

#library(caTools)
#bargif <- read.gif('COVID.gif')

########################################################

# Define UI for application
ui <- fluidPage(
   
   # Application title
   headerPanel("COVID-19 PANDEMIC"),
   br(),
   p("Diagrams illustrating the increasing of COVID-19 case numbers in different countries."),
   br(),
   
#   downloadLink("testgif", label = "EVOLUTION GIF"),
   plotOutput("TOTAL"),
   hr(),
   plotOutput("Evolution"),
   hr(),
   plotOutput("Percentual"),
   hr(),
   p("The map below illustrates the countries with highest confirmed cases of 
     COVID-19"),
   img(src="map.png"),
   hr(),
   p("Data relative to each Country"),
   
    sidebarPanel(
       selectInput("countryInput", "Country",
                   choices = Countries, selected = "")
    ),
      
      mainPanel(
        tabsetPanel(
          tabPanel("Contamined", plotOutput("graph")),
          tabPanel("Deaths", plotOutput("deaths")),
          tabPanel("New Cases", plotOutput("newcases"))
#          tabPanel("EvolutionTable", dataTableOutput("results"))
          
      )
 ))


# Define server logic 
server <- function(input, output) {
  
  filtered <- reactive({dataset[dataset$Country==input$countryInput, ]})
  
#    output$results <- renderDataTable({
#      filtered() 
#    })
    
    output$TOTAL <- renderPlot({
      ggplot()+
        geom_area(mapping = aes(x=total_confirmed$Date, y=total_confirmed$Confirmed), stat = "identity", fill = "darkblue")+
        geom_area(mapping = aes(x=total_death$Date, y=total_death$Deaths*coeff), fill = "red")+
        theme_bw(base_size = 22) +
        scale_y_continuous(name="Confirmed Cases", labels = scales::comma,
                           sec.axis = sec_axis(~ . * 0.1, name = "Deaths",labels = scales::comma))+
        ggtitle("CONFIRMED (blue) and DEATH (red) CASES OF COVID-19")+
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
    
    output$Evolution <- renderPlot({
      ggplot()+
        geom_bar(mapping = aes(x=day_confirmed$Date, y=day_confirmed$Day_Confirmed), stat = "identity", fill = "darkblue")+
        theme_bw(base_size = 22) +
        ggtitle("Confirmed cases of COVID-19 in each day")+
        ylab("New Cases")+
        xlab("")
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
        scale_y_continuous(name="Confirmed Cases", labels = scales::comma)+
        ggtitle("CONFIRMED CASES OF COVID-19")+
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
    
    filtered2 <- reactive({eachday[eachday$Country==input$countryInput, ]})
    
    output$newcases <- renderPlot({
      ggplot(filtered2(), aes(x=Date, y=diary))+
        geom_bar(stat = "identity", fill = "darkblue")+
        theme_bw(base_size = 25) +
        ggtitle("DEATHS FROM COVID-19")+
        ylab("Deaths Cases")+
        xlab("")
      
    })
    
#    output$testgif <- downloadHandler(
#      filename = function(){
#        paste('COVID-', content = Sys.Date(), contentType = "gif")
#      },
#      content = function(file) {
#        read.gif(COVID, file)
#      }
#    )
        
}

# Run the application 
shinyApp(ui = ui, server = server)

