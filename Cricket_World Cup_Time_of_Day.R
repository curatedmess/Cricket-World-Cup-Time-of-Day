# tidytuesday Week11-30-2021 | Cricket
# Data: The data this week comes from ESPN Cricinfo by way of Hassanasir.

# install.packages("devtools")
devtools::install_github("thomasp85/patchwork")

#libraries
library(tidytuesdayR)
library(tidyverse)
library(showtext)
library(patchwork)

## Add Font
font_add_google(name = "Space Mono", family = "Space Mono")

# turn on showtext
showtext_auto()

#load data
tuesdata <- tidytuesdayR::tt_load('2021-11-30')
matches <- tuesdata$matches

view(matches)

#p1 - data wrangle
plot_day <- 
matches %>%
  filter(time_of_day == "Day") %>%
  select(time_of_day, team1, team2, team1_away_or_home, team2_home_away, winner, match_date) %>%
  mutate(
      winning_team = if_else(winner == team1, "team1", "team2"),
      result = if_else(
        (winning_team == "team1" & team1_away_or_home == "home") |
        (winning_team == "team2" & team2_home_away == "home"),
        "Home Wins","Away Wins")
        ) %>%
  count(result) %>%
  mutate(prop = n / sum(n)) %>%
  mutate(rounded = round(prop, 2))

view(plot_day)

#p2 - data wrangle
plot_dayandnight <- 
  matches %>%
  filter(time_of_day == "Day and night") %>%
  select(time_of_day, team1, team2, team1_away_or_home, team2_home_away, winner, match_date) %>%
  mutate(
    winning_team = if_else(winner == team1, "team1", "team2"),
    result = if_else(
      (winning_team == "team1" & team1_away_or_home == "home") |
        (winning_team == "team2" & team2_home_away == "home"),
      "Home Wins","Away Wins")
  ) %>%
  count(result) %>%
  mutate(prop = n / sum(n)) %>%
  mutate(rounded = round(prop, 2))

view(plot_dayandnight)

#p1
p1 <-plot_day %>%
      ggplot(aes(x=result, y=rounded)) +
      coord_cartesian(ylim = c(0, .7)) +
            geom_col(fill = "#FFFFFF") +
            scale_y_continuous(expand = expansion(mult = c(0, .1))) +
            geom_text(aes(label = rounded), vjust = 2, color = "#121212", family="Space Mono", size = 4) +
            #geom_text(aes(label = rounded), position = position_dodge(0.5), vjust = 0) +
            theme_classic() +
            theme(text = element_text(family = "Space Mono", color = "#FFFFFF"),
                  axis.line.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.text.y = element_blank(),
                  axis.title.y = element_blank(),
                  axis.text = element_text(size = 11, 
                              color = "#FFFFFF"),
                  axis.line = element_line(color = "#FFFFFF")) +
                  labs(x = "Day Match")

#p2
p2<- plot_dayandnight %>%
      ggplot(aes(x=result, y=rounded)) +
      coord_cartesian(ylim = c(0, .7)) +
          geom_col(color = "#FFFFFF", fill = "#121212") +
          scale_y_continuous(expand = expansion(mult = c(0, .1))) +
          geom_text(aes(label = rounded), vjust = 2, color = "#FFFFFF", family="Space Mono", size = 4) +
          #geom_text(aes(label = rounded), position = position_dodge(0.5), vjust = 0) +
          theme_classic() +
          theme(text = element_text(family = "Space Mono", color = "#FFFFFF"),
                axis.line.y = element_blank(),
                axis.ticks.y = element_blank(),
                axis.text.y = element_blank(),
                axis.title.y = element_blank(),
                axis.text = element_text(size = 11, 
                                         color = "#FFFFFF"),
                axis.line = element_line(color = "#FFFFFF")) +
          labs(x = "Day-Night Match")

#combine plots

final <- p1 | p2

final + plot_annotation(title = "Cricket World Cup 1996 to 2005",
                        subtitle = "Does the match time of day favor the Away or Home Team?",
                        caption = "\n#tidytuesday | Data: ESPN Cricinfo | Design: Ryan Hart") &
            theme(text = element_text(family = "Space Mono", color = "#FFFFFF"),
                        plot.title = element_text(size = 16, family = "Space Mono" , hjust = .5, margin = margin(t = 10, b = 5)),
                        plot.subtitle = element_text(size = 11, family = "Space Mono", margin = margin(t = 15, b = 15), hjust = .5, lineheight = 1.25),
                        plot.background = element_rect(fill = "#121212", color = NA),
                        panel.background = element_rect(fill = "#121212", color = NA),
                        plot.margin =  margin(30, 50, 50, 30),
                        plot.caption = element_text(size = 8, hjust = .5, margin = margin(b = .5, unit = "cm"))
) 


#save
ggsave("time of day.png", units = "in", width=8, height=6, dpi = 320)



