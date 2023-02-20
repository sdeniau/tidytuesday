library(tidyverse)
library(zoo)
library(ggtext)
library(showtext)
library(here)
library(lubridate)
library(grid)
library(geomtextpath)

#load google fonts
font_add_google("EB Garamond", "subtitle", regular.wt = 400)
font_add_google("Fira Sans Condensed", "title", regular.wt = 800)
font_add_google("Fira Sans", "text", regular.wt = 400)
font_add_google("Fira Sans", "label", regular.wt = 500)
font_add_google("Fira Mono", "axis", regular.wt = 400)
showtext_auto()

#set theme
theme_set(theme_minimal())
theme_update(
        text = element_text(family = "text"),
        # title = element_text("title", size = 22, color = "gray10"),
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

age_gaps <-
        readr::read_csv(
                'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-14/age_gaps.csv'
        )

data <- age_gaps %>%
        filter(character_1_gender != character_2_gender) %>%
        mutate(
                diff = case_when(
                        character_1_gender == "man" ~ actor_1_age - actor_2_age,
                        character_1_gender == "woman" ~ actor_2_age - actor_1_age
                )
        ) %>%
        select(movie_name, release_year, diff)

datayearmedian <- data %>%
        group_by(release_year) %>%
        mutate(median = median(diff))

illustr <- age_gaps |> 
        arrange(desc(age_difference)) |> 
        slice_head(n=2) |> 
        select(movie_name,release_year,
               actor_1_name,character_1_gender,actor_1_age,
               actor_2_name,character_2_gender,actor_2_age) |> 
        mutate(txt = paste (
                "<b><i>",movie_name,"</i></b> (",release_year,")<br>",
                actor_1_name," - ",actor_1_age,"yo<br>",
                actor_2_name," - ",actor_2_age,"yo",sep=""
        )) |> 
        mutate(x=release_year-25)

illustr$fill <- c("#0057b7","#ffd700")
illustr$y <- c(-40,47)
illustr$yt<-c(-52,50)



title <- "Age gaps (<span style = 'color:#ffd700;'>Man</span> -
<span style = 'color:#0057b7;'>Woman</span>)
in opposite-sex couples in movies" 
subtitle <- "by year, for 1132 movies from 1935 to 2022"
xlab <- NULL
ylab <- "Woman is older by            Man is older by       "
caption <- "Source: Hollywood Age Gap via Data Is Plural. 
<i>Gender values appear to indicate how the characters in each film identify. 
Some of these values do not match how the actor identifies.</i>"


plot <- ggplot(data, aes(x = release_year, y = diff)) +
        geom_point(
                data = data %>% filter(abs(diff) == 0),
                size = 3,
                alpha = .4,
                color = "grey"
        ) +
        geom_point(
                data = data %>% filter (diff > 0),
                alpha = .4,
                size = 3,
                color = "#ffd700"
        ) +
        geom_point(
                data = data %>% filter (diff < 0),
                size = 3,
                alpha = .4,
                color = "#0057b7"
        ) +
        geom_textsmooth(data=datayearmedian,
                aes(x = release_year, y = median),
                family="label",
                size=5,
                label="age gap average",
                color="grey20",
                se=FALSE,
        ) +
        labs(
                title = title, 
                subtitle = subtitle,
                x = xlab,
                y = ylab,
                caption = caption
        ) +
        geom_richtext(data = illustr,
                      aes(x=x, y=y, 
                          label = txt, fill=fill, alpha=0.4),
                      family="text",
                      size=5,
                     # color="#ff0000",
                      lineheight = 1.1
                      ) +
        scale_x_continuous(limits=c(1930,2027),n.breaks=7, 
                           expand = c(0, 0)) +
        scale_y_continuous(limits=c(-60,+60),n.breaks=7,
                           label=c("60y","40y","20y","0y","20y","40y","60y"),
                           expand = c(0, 0)) +
        scale_fill_identity() +
        annotate(
                geom = "curve",
                x = 1971, xend = 1958.8,
                y = -51, yend = -40,
                curvature = .3,
                color="grey20"
                )+
        annotate(
                geom = "curve",
                x = 2006, xend = 1992.5,
                y = 51, yend = 47,
                curvature = .3,
                color="grey20"
        ) + 
        geom_point(data=illustr,aes(x=release_year,y=yt), 
                   color="grey20",
                   size=3,
                   shape=1)
        
png("plotgap.png", width = 890, height = 541)

plot

grid.lines(x = c(0, 1),
           y = 0.99,
           gp = gpar(col = "grey40", lwd = 4))

grid.lines(x = c(0, 1),
           y = 0.01,
           gp = gpar(col = "grey40", lwd = 2))

dev.off()
