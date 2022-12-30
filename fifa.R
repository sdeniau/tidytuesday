library(tidyverse)
library(ggimage)
library(showtext)

matches <- read_csv("wcmatches.csv",
                    col_types = "cccccciiccccTcc")

data <- matches %>% select(year,stage,home_team,away_team,winning_team,losing_team,date)

data1 <- data %>% group_by(year) %>% 
            mutate(days=difftime(date,min(date),units="days")) %>% 
            mutate(days=as.integer(days)) %>% 
            filter(year>1957) %>% 
            filter(away_team=="France" | home_team=="France") %>% 
            mutate(results=case_when(
                        winning_team=="France"~"W",
                        losing_team=="France"~"L",
                        is.na(winning_team)~"D"
            )) %>% 
            mutate(adv=case_when(
                        away_team=="France"~home_team,
                        home_team=="France"~away_team
            )) %>% 
            select(year,stage,adv,days,results) %>% 
            mutate(icon=paste("flags/",tolower(adv),".png",sep=""))

font_add_google("Roboto", "RobotoThin", regular.wt = 100) 
font_add_google("Roboto", "RobotoLight", regular.wt = 300) 
font_add_google("Roboto", "RobotoRegular", regular.wt = 400)
font_add_google("Roboto", "RobotoMedium", regular.wt = 500)
font_add_google("Roboto", "RobotoBold", regular.wt = 700)
font_add_google("Roboto", "RobotoBlack", regular.wt = 900)

showtext_auto()

ggplot(data1,aes(x=year,y=days)) +
            geom_linerange(aes(ymin = 0, ymax = days),size=2, color="grey")+
            geom_point(aes(color=results), size=12) +
            geom_image(aes(x = year, y = days,
                        image = icon), asp = 1.8, size=.025)+
            geom_text(aes(label=stage),vjust=3.2, family="RobotoThin", size=3)+
            scale_color_manual(values=c("W"="green","L"="red","D"="darkgrey")) +
            theme_minimal() +
            theme(legend.position="none",
                  panel.grid.major.y = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  panel.background = element_rect(fill="lightgrey", 
                                                  color="white"),
                  plot.title = element_text(family = "RobotoBlack", 
                                            color ="black", size=20),
                  plot.subtitle = element_text(family = "RobotoLight", 
                                            face="italic", color ="black", 
                                            size=16),
                  axis.title.x = element_text(family="RobotoBold", size=13,
                                              margin = margin (t=3)),
                  axis.title.y = element_text(family="RobotoBold", size=13,
                                              margin = margin (r=13)),
                  axis.line.x.bottom =  element_line(linewidth =7, 
                                                       color="black"),
                  axis.text.x = element_text(vjust = 4, 
                                             family="RobotoBold", size=13,
                                             color="white"),
                  axis.text.y = element_text(family="RobotoBold", size=11),
                  plot.caption = element_text(family="RobotoLight", size=10)
                  ) +
            coord_flip() +
            scale_y_continuous(limits=c(0,33),breaks=c(0,7,14,21,28)) +
            scale_x_discrete(expand =c(0.1,0)) +
            labs(y="#days since the beginning of the tournament",
                 x="Year of the World Cup",
                 title="Course of the French team in FIFA World Cup",
                 subtitle="From 1958 to 2018",
                 caption = "Data from Kaggle ; flag icons from Freepik on flaticon.com")
