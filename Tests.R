library(dplyr)

#population <- read.csv('POP.csv', stringsAsFactors = F, header = TRUE)

#population <- population %>% 
#  select(-X, -X.1, -X.2)

#colnames(population) <- c("Ranking", "Country", "Population")

#population$Population <- as.numeric(gsub(",", "", population$Population))

#population2 <- population %>% 
#  select(Population)

#population$Population <- (population$Population)*1000

#newdat <- population[order(population$Country),]

#conferename<- as.data.frame(Countries)

#confere <- conferename[order(conferename),]

#result <- merge(data.frame(newdat, row.names=NULL), data.frame(confere, row.names=NULL), by = 0, all = TRUE)[-1]

#write.csv(population, file="WPopulation.csv", quote = T, row.names=F)

test <- read.csv('~/Dropbox/Covid19/Covid19/WPopulation.csv', stringsAsFactors = F, header = TRUE)

test3 <- aggregate(dataset$Confirmed, by = list(dataset$Country), max)

colnames(test3) <- c("Country", "Confirmed")

test2 <- merge(test, test3, by="Country" )

test2$Percentual <- (test2$Confirmed/test2$Population)*100

test4 <- test2 %>%
  mutate(rank=rank(-Percentual), 
         Value_rel=Percentual/Percentual[rank==1],
         Value_lbl = paste0("", Percentual)) %>%
  filter(rank<=20)

test4 <- test4[order(-test4$rank),]

ggplot(test4, aes(x=Country, y=Percentual))+
  geom_bar(stat = "identity", fill = "darkblue")+
  theme_bw(base_size = 15) +
  ggtitle("% OF CONFIRMED CASES X TOTAL POPULATION")+
  ylab("% of total population")+
  xlab("Country")+
  coord_flip()+
  ggsave("plot.png", width = 8, height = 5)

##############################theme(axis.text.x=element_text(angle = 90, hjust = 0))+

library(tibbletime)
library(dplyr)
library(lubridate)
library(tidyverse)

dataset <- dataset %>% 
  select(-Recovered)

#dataset2 <- dataset %>%
#  mutate(year = year(Date), month = month(Date), day = day(Date))

#dataset2 <- dataset2 %>%
#  select(-Date, -year)

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
library(png)
  
anim = ggplot(dataset4, aes(rank, group = Country, fill = as.factor(Country), 
                            color = as.factor(Country)))+
  geom_tile(aes(y = Confirmed/2,
                height = Confirmed,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(Country, " ")), vjust = 0.2, hjust = 1, size = 8) + #determine size of the Nationlity label
  geom_text(aes(y=Confirmed,label = Value_lbl, hjust=0),size = 9 ) +  #determine size of the value label
  coord_flip(clip = "off", expand = TRUE) +
  scale_x_reverse() +
  theme_bw() +
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
        plot.title=element_text(size=40, hjust=0.5, face="bold",     colour="black", vjust=-1),
        plot.subtitle=element_text(size=30, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=20, hjust=0.5, face="italic", color="black"),
        plot.background=element_blank(),
        plot.margin = margin(1,4, 1, 8, "cm")) +
  transition_states(Date, transition_length = 3, state_length = 1, wrap = FALSE) +
  ease_aes('linear') + enter_fade() + exit_shrink() +
  view_follow(fixed_x = TRUE) +
  labs(title = 'COVID19 Confirmed Cases: {closest_state}', 
       subtitle = "Top 10 Countries",
       caption  = "By Anderson Hoff - Data Source: https://pomber.github.io/covid19/timeseries.json") 

animate(anim, nframes = 500,fps = 10,  width = 1200, height = 1000, 
        renderer = gifski_renderer("gganim6.gif"), end_pause = 40,
        start_pause = 0)

for_mp4 <- animate(anim, nframes = 200,fps = 10,  width = 1200, height = 1000, 
                   renderer = ffmpeg_renderer())

anim_save("covid.mp4", animation = for_mp4)
                   
######################

max_confirmed <- aggregate(dataset$Confirmed, by = list(dataset$Country), max)

colnames(max_confirmed) <- c("Country", "Confirmed")

library(rworldmap)

#create a map-shaped window
mapDevice('x11')
#join to a coarse resolution map
spdf <- joinCountryData2Map(max_confirmed, joinCode="NAME", nameJoinColumn="Country")

mapCountryData(spdf, nameColumnToPlot="Confirmed", numCats = 16,catMethod="fixedWidth", 
               colourPalette="diverging")

savePlot(filename=paste0("map.png"),type="png")
dev.off()
