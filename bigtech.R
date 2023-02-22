library(tidyverse)
library(zoo)
library(ggtext)
library(showtext)
library(here)
library(lubridate)
library(grid)
library(geomtextpath)
library(rvest)
library(ggimage)
library(cropcircles)
library(magick)


#load google fonts
font_add_google("EB Garamond", "subtitle", regular.wt = 400)
font_add_google("Fira Sans Condensed", "title", regular.wt = 800)
font_add_google("Fira Sans", "text", regular.wt = 400)
font_add_google("Fira Sans", "label", regular.wt = 500)
font_add_google("Fira Mono", "axis", regular.wt = 400)
showtext_auto()


# Set theme
theme_set(theme_minimal())
theme_update(
        text = element_text(family = "text"),
        plot.title = element_markdown(family = "title",
                                      size = 24,
                                      color = "gray10"),
        plot.title.position = "plot",
        plot.subtitle = element_markdown(
                family= "subtitle",
                face = "italic",
                size = 18,
                margin = margin(0, 0, 15, 0),
                color = "gray40"
        ),
        axis.text = element_text(family="axis", size = 14),
        axis.title = element_text(family="label", size = 15),
        axis.line = element_line(color = "gray60"),
        panel.grid.major = element_line(color = "gray80"),
        panel.grid.minor = element_line(color = "gray90"),
        plot.margin = margin(15, 5, 11, 5),
        plot.background = element_rect(
                fill = "#E3E4FF", 
                color = "#E3E4FF"
                ),
        plot.caption = element_textbox_simple(
                family="text",
                lineheight = 1,
                size = 12,
                color = "grey40",
                margin = margin(10, 0, 0, 0),
                hjust = 0
        ),
        plot.caption.position = "plot",
        legend.position = "none"
)

#download tech company list and enrich with logos
#had to convert svg in png in order to use crop_circle 
tech_comp <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_companies.csv")

for (i in 1:14){
        print(i)
        test<-try(url <- read_html(
                paste("https://fr.tradingview.com/symbols/NASDAQ-",
                      tech_comp$stock_symbol[i],"/",sep="")))
        if("try-error" %in% class(test)) {url <- read_html(
                paste("https://fr.tradingview.com/symbols/NYSE-",
                      tech_comp$stock_symbol[i],"/",sep=""))}
        logohtml <- url %>% html_nodes(".tv-category-header__icon") 
        logourl <- gsub('.*src=\"',"",logohtml)
        logourl <- gsub('".*',"",logourl)
        svg <- image_read_svg(logourl,width=500)
        png <- paste("logo/",tech_comp$stock_symbol[i],".png",sep="")
        image_write(svg,path=png,format="png")
        xxx<-image_read(circle_crop(png, border_colour = "yellow",border_size = 20))
        image_write(xxx,path=png,format="png")
        tech_comp$logo[i] <- png
}


tech_stock <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-07/big_tech_stock_prices.csv")

tech_time <- tech_stock |> 
        filter(date>"2021-12-31" & date<"2023-01-01") |> 
        group_by(stock_symbol) |> 
        mutate(start=open[date=="2022-01-03"]) |> 
        mutate(value=1000*(close/start)) |> 
        mutate(gain = value-1000)

tech_time <- tech_time |> left_join(tech_comp)

tech_last <- tech_time |> 
        mutate(date=as.Date(date)) |> 
        group_by(stock_symbol) |> 
        arrange(desc(date)) |> 
        slice_head(n=1) |> 
        mutate(lastgain=round(gain)) |> 
        mutate(gaintext=case_when(
                lastgain>0~paste(" +$",lastgain,sep=""),
                lastgain<0~paste("-$",abs(lastgain),sep="")
        )) |> 
        select(stock_symbol,lastgain,gaintext)

tech_time <- tech_time |> left_join(tech_last)

tech_time$stock_symbol<-factor(tech_time$stock_symbol)
tech_time$stock_symbol<-fct_reorder(tech_time$stock_symbol, tech_time$gain)

title <- "How much would I have earned by the end of 2022?" 
subtitle <- "if I had invested $1,000 beginning of 2022"
xlab <- NULL
ylab <- NULL
caption <- "Source: Yahoo Finance via Kaggle (by Evan Gower)"


plot <- ggplot(tech_time,aes(x=date,y=value, group=stock_symbol)) +
        geom_area(linewidth=1,color="white", fill="grey10")+
        geom_image(aes(x=as.Date("2022-02-25"), y=200,
                       image=logo),size=0.3, by="height") +
        scale_size_identity()+
        geom_text(data=tech_last |> filter(lastgain>0), 
                  aes(x=as.Date("2022-10-01"), family="title",
                        y=180,label=gaintext),color="#00FF00", size=9)+
        geom_text(data=tech_last|> filter(lastgain<0), 
                  aes(x=as.Date("2022-10-01"), family="title",
                      y=180,label=gaintext),color="#FF0000", size=9)+
        facet_wrap(~fct_reorder(stock_symbol,lastgain)) +
        scale_x_continuous(limits=c(min(tech_time$date),
                                    max(tech_time$date), n.breaks=1,
                                    label=c("01/01","07/01","12/31")),
                           expand = c(0, 0)) +
        scale_y_continuous(limits=c(0,1100, n.breaks=1),
                           expand = c(0, 0)) +
        theme(aspect.ratio = 1,
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              axis.ticks.x = element_blank(),
              axis.text.x = element_blank(),
              strip.background = element_blank(),
              strip.text.x = element_blank()) +
        labs(
                title = title, 
                subtitle = subtitle,
                x = xlab,
                y = ylab,
                caption = caption
        )



png("plotgap2.png", width = 815, height = 890)

plot

grid.lines(x = c(0, 1),
           y = 0.99,
           gp = gpar(col = "grey40", lwd = 4))

grid.lines(x = c(0, 1),
           y = 0.01,
           gp = gpar(col = "grey40", lwd = 2))

dev.off()
