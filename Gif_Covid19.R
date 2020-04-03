library(tibbletime)
library(dplyr)
library(magrittr)
library(reshape2)
library(jsonlite)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(gganimate)
library(gifski)
library(png)

###############################
url <- "https://pomber.github.io/covid19/timeseries.json"
destfile <- "timeseries.json"
download.file(url, destfile,method = "curl", mode="wb")

datajson <- jsonlite::fromJSON('timeseries.json')

covidata <- reshape2::melt(datajson, id.vars = c("date", "confirmed","deaths","recovered"))

colnames(covidata) <- c("Date","Confirmed",  "Deaths", "Recovered","Country")

covidata$Confirmed <- as.integer(covidata$Confirmed)

covidata$Deaths <- as.integer(covidata$Deaths)

covidata$Recovered <- as.integer(covidata$Recovered)

Countries <- as.factor(unique(covidata$Country))

covidata <- covidata[order(covidata$Country, -covidata$Confirmed),]

covidata$Date <- as.Date(covidata$Date, format = "%Y-%m-%d")

covidata <- covidata %>% 
  select(-Recovered) %>%
  group_by(Date) %>%
  mutate(rank=rank(-Confirmed), 
         Value_rel=Confirmed/Confirmed[rank==1],
         Value_lbl = paste0("", Confirmed)) %>%
  group_by(Country) %>%
  filter(rank<=10)

anim = ggplot(covidata, aes(rank, group = Country, fill = as.factor(Country), 
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
        renderer = gifski_renderer("Covid19.gif"), end_pause = 40,
        start_pause = 0)

##########
#for_mp4 <- animate(anim, nframes = 200,fps = 10,  width = 1200, height = 1000, 
#                   renderer = ffmpeg_renderer())

#anim_save("covid.mp4", animation = for_mp4)
########