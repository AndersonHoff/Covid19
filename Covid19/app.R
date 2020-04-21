############### App of COVID-19 evolution ######################

library(shiny)
library(dplyr)
library(ggplot2)
library(magrittr)
library(reshape2)
library(jsonlite)
library(shinythemes)

###############################
url <- "https://pomber.github.io/covid19/timeseries.json"
destfile <- "timeseries.json"
download.file(url, destfile,method = "curl", mode="wb")

datajson <- jsonlite::fromJSON('timeseries.json')

dataset <- reshape2::melt(datajson, id.vars = c("date", "confirmed","deaths",
                                                "recovered"))

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

deathday <- dataset[order(dataset$Country, dataset$Confirmed),]
deathday <- deathday %>%
  select(-Recovered) %>%
  group_by(Country) %>%
  mutate(diarydeaths = Deaths - lag(Deaths, default = 0))

###############################################################

total_confirmed <- aggregate(dataset$Confirmed, by=list(Category=dataset$Date), 
                             FUN=sum)
colnames(total_confirmed) <- c("Date","Confirmed")

total_death <- aggregate(dataset$Deaths, by=list(Category=dataset$Date), FUN=sum)
colnames(total_death) <- c("Date","Deaths")

day_confirmed <- aggregate(eachday$diary, by=list(Category=eachday$Date), FUN=sum)
colnames(day_confirmed) <- c("Date","Day_Confirmed ")

day_death <- aggregate(deathday$diarydeaths, by=list(Category=deathday$Date), FUN=sum)
colnames(day_death) <- c("Date","Day_Death ")

daydata <- merge(day_confirmed, day_death, by="Date")
daydata$lethal <- (daydata$Day_Death/daydata$Day_Confirmed)*100
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

max_death <- aggregate(dataset$Deaths, by = list(dataset$Country), max)
colnames(max_death) <- c("Country", "Deaths")
deaths_max <- merge(population, max_death, by = "Country")
deaths_max$Percentual <- (deaths_max$Deaths/deaths_max$Population)*100

deaths_max <- deaths_max %>%
  mutate(rank=rank(-Percentual),
         Value_rel=Percentual/Percentual[rank==1],
         Value_lbl = paste0("", Percentual)) %>%
  filter(rank<=20)

deaths_max <- deaths_max[order(deaths_max$rank),]

maximum <- merge(max_confirmed, max_death, by = "Country")

maximum$Lethality <- (maximum$Deaths/maximum$Confirmed)*100
maximum <- maximum %>%
  mutate(rank=rank(-Lethality),
         Value_lbl = paste0("", Lethality))  %>%
  filter(rank<=20)
#############################

#library(rworldmap)

######### Confirmed #########
#mapDevice('x11')

#spdf <- joinCountryData2Map(max_confirmed, joinCode="NAME", 
#                            nameJoinColumn="Country")

#mapCountryData(spdf, nameColumnToPlot="Confirmed", numCats = 20,
#               catMethod="logFixedWidth", 
#               colourPalette="diverging")

#savePlot(filename=paste0("www/logmap.png"),type="png")
#dev.off()

####### Deaths ########
#mapDevice('x11')
#spdf <- joinCountryData2Map(max_death, joinCode="NAME", 
#                            nameJoinColumn="Country")

#mapCountryData(spdf, nameColumnToPlot="Deaths", numCats = 20,
#               catMethod="logFixedWidth", 
#               colourPalette="diverging")

#savePlot(filename=paste0("www/deaths.png"),type="png")
#dev.off()

rm(url,spdf, destfile, datajson)
########################################################

#library(caTools)
#bargif <- read.gif('www/Covid19.gif')

########################################################

# Define UI for application
ui <- navbarPage(title ="COVID-19: Diagrams illustrating the 
                        increasing of COVID-19 numbers in 
                        the world",
   
   # Application title
#   headerPanel(h1("COVID-19 PANDEMIC", align="center")),
#   br(),
#   p(h2("Diagrams illustrating the increasing of COVID-19 numbers in 
#     different countries.", align="center")),
#   br(),div(src="sars-cov-19.jpg")
#   downloadLink("testgif", label = "EVOLUTION GIF"),
   tabsetPanel(type = "pills",
      
      tabPanel(title = "EVOLUTION",
        hr(),
        p(h4("Evolution of the total number of cases around the World", 
             align="center")),
        hr(),
        plotOutput("TOTAL"),
        hr(),
        plotOutput("Evolution")
        ),
     
      tabPanel(title = "SELECT A COUNTRY", 
               hr(),  
               p("Data relative to each Country. Select the respective Country in 
     the panel."),
               
               sidebarPanel(
                 selectInput("countryInput", "Country",
                             choices = Countries, selected = "")
               ),
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("Contamined", plotOutput("graph")),
                   tabPanel("Deaths", plotOutput("deaths")),
                   tabPanel("New Cases", plotOutput("newcases")),
                   tabPanel("New Deaths", plotOutput("newdeaths"))
                 ))),
      
     tabPanel(title = "STATISTICS", 
        hr(),        
        p("The results are calculated related to the total population 
          of each country."),
        plotOutput("Percentual"),
        hr(),
        plotOutput("Percentual2"),
        hr(),
        p("The lethality in each country (the ratio between people who 
          died from the total that got the virus."),
        plotOutput("lethal")
        ),
     
     tabPanel(title = "WORLD MAPS",   
   p("The map below illustrates the countries with highest confirmed cases of 
     COVID-19. Since the USA has a high number of cases, it makes difficult 
     to see other countries in this scale. So I used a log palette scale to 
     makes the differences between other countries visible."),
   div(img(src="logmap.png"), style="text-align: center;"),
   hr(),
   div(img(src="deaths.png"), style="text-align: center;")
   ),
   
     tabPanel(title = "GIF",
              hr(),
              img(src="Covid19.gif", align = "left", height='600px',width='700px')
              ),
   
    tabPanel("ABOUT",
             hr(),
             p("Write..."))
   ))

# Define server logic 
server <- function(input, output) {
  
  filtered <- reactive({dataset[dataset$Country==input$countryInput, ]})
    
    output$TOTAL <- renderPlot({
      ggplot()+
        geom_area(mapping = aes(x=total_confirmed$Date, 
                                y=total_confirmed$Confirmed), stat = "identity", 
                  fill = "darkblue")+
        geom_area(mapping = aes(x=total_death$Date, y=total_death$Deaths*coeff), 
                  fill = "red")+
        theme_bw(base_size = 22) +
        scale_y_continuous(name="Confirmed Cases", labels = scales::comma,
                           sec.axis = sec_axis(~ . * 0.1, name = "Deaths",
                                               labels = scales::comma))+
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
        geom_col(mapping = aes(x=daydata$Date, 
                               y=daydata$Day_Confirmed), 
                 fill = "darkblue")+
        geom_col(mapping = aes(x=daydata$Date, y=daydata$Day_Death*coeff/2), 
                  fill = "red")+
        theme_bw(base_size = 22) +
        scale_y_continuous(name="Confirmed Cases", labels = scales::comma,
                           sec.axis = sec_axis(~ . * 0.2, name = "Deaths",
                                               labels = scales::comma))+
        ggtitle("Confirmed (blue) and death (red) cases of COVID-19 in each day")+
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
      ggplot(max_country, aes(x=reorder(Country, Percentual), y=Percentual))+
        geom_bar(stat = "identity", fill = "darkblue")+
        theme_bw(base_size = 17) +
        ggtitle("% OF CONFIRMED CASES X TOTAL POPULATION (20 highest rates)")+
        ylab("% of total population")+
        xlab("Country")+
        coord_flip()
    })
    
    output$Percentual2 <- renderPlot({
      ggplot(deaths_max, aes(x=reorder(Country, Percentual), y=Percentual))+
        geom_bar(stat = "identity", fill = "red")+
        theme_bw(base_size = 17) +
        ggtitle("% OF DEATHS X TOTAL POPULATION (20 highest rates)")+
        ylab("% of total population")+
        xlab("Country")+
        coord_flip()
    })
    
    output$lethal <- renderPlot({
      ggplot(maximum, aes(x=reorder(Country, Lethality), y=Lethality))+
        geom_bar(stat = "identity", fill = "red")+
        theme_bw(base_size = 17) +
        ggtitle("LETHALITY INDEX (20 highest rates)")+
        ylab("Lethality Index (%)")+
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
        geom_bar(stat = "identity", fill = "red")+
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
        ggtitle("NEW CONFIRMED CASES FOR EACH DAY")+
        ylab("New Confirmed Cases")+
        xlab("")
      
    })
    
    filtered3 <- reactive({deathday[deathday$Country==input$countryInput, ]})
    
    output$newdeaths <- renderPlot({
      ggplot(filtered3(), aes(x=Date, y=diarydeaths))+
        geom_bar(stat = "identity", fill = "red")+
        theme_bw(base_size = 25) +
        ggtitle("NEW DEATHS FOR EACH DAY")+
        ylab("New Death Cases")+
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

