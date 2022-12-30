library(tidyverse)
library(waffle)
library(ggtext)
library(showtext)

#download Star Trek dataset
tlBooks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-27/tlBooks.csv')

#download futuristic fonts 
font_add_google("Orbitron", "light", regular.wt = 400) 
font_add_google("Orbitron", "regular", regular.wt = 600) 
font_add_google("Orbitron", "black", regular.wt = 800)
showtext_auto()

#create dataframe linking series titles abbreviations 
#with series titles
totalabb <-c(
        "Deep Space Nine","DS9",       
        "Enterprise","ENT",       
        "New Frontier","NF",        
        "Starfleet Corps of Engineers","SCE",       
        "Stargazer","SGZ",       
        "The Lost Era","TLE",       
        "The Next Generation","TNG",       
        "The Original Series","TOS",       
        "Titan","TTN",       
        "Voyager","VOY",
        "The Animated Series","TAS"
        )
totalabb <- matrix(totalabb,11,2,byrow=TRUE)
totalabb <- as.data.frame.array(totalabb)
names(totalabb) <- c("seriestitle","series")

#create dataframe for plot
tlBooks |> 
        left_join(totalabb) |> 
        select(title,seriestitle, format) |> 
        unique() |> 
        filter(!is.na(seriestitle)) |> 
        group_by(seriestitle,format) |> 
        summarise(count=n())  -> stseries

#order series by descending order of total items count
stseries$seriestitle<-fct_rev(fct_reorder(
        stseries$seriestitle, 
        stseries$count, .fun=sum))

#select specific star trek colors 
#from https://color.adobe.com/Star-Trek-(RetroMoviePostercom)-color-theme-6247559/
stcolors <- c("#A83252","#ECD472","#547139")

#plot
title <- paste("STAR TREK Universe: number of <span style='color:",
                stcolors[1],
                ";'>Books</span>,<span style='color:",
                stcolors[2],
               ";'> Episodes</span> and <span style='color:",
               stcolors[3],
               ";'>Stories</span> per series ")
caption <- "Data from rtrek package"
xlab <- NULL
ylab <- NULL

png(file="/home/sylvain/Downloads/startrek.png",
    width=948, height=560)

ggplot(stseries, aes(fill=format, values=count)) +
        geom_waffle(color = "black", size=.15, n_rows = 3) +
        facet_wrap(~seriestitle, ncol=1) +
        scale_x_continuous(labels = function(x) x * 3,
                           expand=c(0,0), limits=c(0,145),
                           n.breaks = 10) +
        scale_y_discrete(expand=c(0,0)) +
        scale_fill_manual(values=stcolors) +
        theme_minimal() +
        coord_equal() +
        labs (title=title,
              caption=caption,
              x=xlab,
              y=ylab) +
        theme(plot.background = element_rect(fill="black"),
                legend.position = "none",
              panel.grid = element_blank(), 
                    axis.ticks.y = element_line(),
              strip.background = element_rect(fill="black"),
              strip.text.x = element_text(
                      size = 9, color = "white", 
                      family = "black",
                      hjust=0),
              axis.text.x = element_text(color = "white",
                                         size=9, 
                                         family="regular"),
              plot.title = ggtext::element_markdown(
                      family = "black",
                      size = 16, color="white"),
              plot.caption = ggtext::element_markdown(
                      family = "light",
                      size = 9, color="white")) 
dev.off()
