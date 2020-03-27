library(dplyr)

population <- read.csv('POP.csv', stringsAsFactors = F, header = TRUE)

population <- population %>% 
  select(-X, -X.1, -X.2)

colnames(population) <- c("Ranking", "Country", "Population")

population$Population <- as.numeric(gsub(",", "", population$Population))

population2 <- population %>% 
  select(Population)

population$Population <- (population$Population)*1000

write.csv(population, file="WPopulation.csv", quote = T, row.names=F)

test <- read.csv('WPopulation.csv', stringsAsFactors = F, header = TRUE)

###########

library(tibbletime)
library(dplyr)
library(lubridate)
library(tidyverse)

dataset <- dataset %>% 
  select(-Recovered, -Deaths)

dataset2 <- dataset %>%
  mutate(year = year(Date), month = month(Date), day = day(Date))

dataset2 <- dataset2 %>%
  select(-Date, -year)

dataset3 <- dataset2 %>%
  group_by(month) %>%
  group_by(day) %>%
  mutate(rank=rank(-Confirmed), 
         Value_rel=Confirmed/Confirmed[rank==1],
         Value_lbl = paste0("", Confirmed)) %>%
  group_by(Country) %>%
  filter(rank<=10)
##########
dataset4 <- dataset %>%
  group_by(Date) %>%
  mutate(rank=rank(-Confirmed), 
         Value_rel=Confirmed/Confirmed[rank==1],
         Value_lbl = paste0("", Confirmed)) %>%
  group_by(Country) %>%
  filter(rank<=10)

library(ggplot2)
library(gganimate)
library(gifski)  
  
anim2 = ggplot(dataset4, aes(rank, group = Country))+
  geom_tile(aes(y = Confirmed/2,
                height = Confirmed,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(Country, " ")), vjust = 0.2, hjust = 1, size = 7) + #determine size of the Nationlity label
  geom_text(aes(y=Confirmed,label = Value_lbl, hjust=0),size = 8 ) +  #determine size of the value label
  coord_flip(clip = "off", expand = TRUE) +
  scale_x_reverse() +
  theme_minimal() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold",     colour="red", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="red"),
        plot.caption =element_text(size=12, hjust=0.5, face="italic", color="red"),
        plot.background=element_blank(),
        plot.margin = margin(1,4, 1, 8, "cm")) +
  transition_states(Date, transition_length = 10, state_length = 2, wrap = FALSE) +
  ease_aes('sine-in-out') +
  labs(title = 'Number of International Students Enrolled in Australia per Year by Nationality: {closest_state}',  
       caption  = "Data Source: https://internationaleducation.gov.au/Pages/default.aspx") 
  
library(png)

animate(anim2, nframes = 150,fps = 10,  width = 1200, height = 1000, 
                    renderer = gifski_renderer("gganim3.gif"))
  
  
  

country_formatted <- dataset %>%
  group_by(Country) %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-Confirmed),
         Value_rel = Confirmed/rank[rank==2],
         Value_lbl = paste0(" ",round(Confirmed/1e9))) %>%
  group_by(Country) %>% 
  filter(rank <=10) %>%
  ungroup()
View(country_formatted)

dataset2 <- dataset %>%
  select(-Recovered)

dataset2 <- dataset[order(dataset$Date, -dataset$Confirmed),]
dataset2 <- dataset2 %>%
  group_by(Date)%>%
  ungroup


dataset2 <- dataset2 %>%
  group_by(day=floor_date(date, "1 day"))%>%
  mutate(rank = rank(-Confirmed),
         Value_rel = Confirmed/Confirmed[rank==1],
         Value_lbl = paste0(" ",Confirmed)) %>%
  group_by((Country))%>%
  filter(rank<=10)

library(lubridate)
group_by(day=floor_date(date, "month"))#%>%
  mutate(rank = rank(-Confirmed))%>%
  group_by((Date))%>%
  filter(rank <=10) %>%
  ungroup()
  
library(tidyverse)
  dec_intake_formatted = dec_intake %>% 
    group_by(Year)%>%      
    mutate(rank = rank(-Students),
           Value_rel = Students/Students[rank==1],
           Value_lbl = paste0(" ",Students)) %>%
    group_by(Nationality) %>%
    filter(rank <= 10)
