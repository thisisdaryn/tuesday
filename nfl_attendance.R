library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(scales)


attendance <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/attendance.csv')
standings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-04/standings.csv')


team_summaries <- group_by(attendance, team_name, year) %>% 
  summarise(home = mean(home, na.rm = TRUE)/8,
            away = mean(away, na.rm = TRUE)/8) %>% 
  ungroup()

year_summaries <- group_by(attendance, year) %>% 
  summarise(league_average = mean(weekly_attendance, na.rm = TRUE)) %>% 
  ungroup()

df <- left_join(team_summaries, year_summaries) 

df <- gather(df, key = "Category", value = "Attendance", home:league_average) %>%
  mutate(Category = if_else(Category == "home", "Home", 
                            if_else(Category == "away", "Away", "League Avg")),
         team_name = if_else(team_name == "Redskins", "Washington", team_name),
         year = year%%2000)

df$Category <- factor(df$Category, levels = c("Home", "Away",
                                              "League Avg"))

ggplot(data = df) + 
  geom_line(aes(x = year, y = Attendance, group = Category, 
                colour = Category)) + 
  facet_wrap(~team_name, ncol = 8, scales = "free_x")  + theme_bw() + 
  theme(axis.text.x = element_text(angle = 90), legend.title = element_blank(),
        legend.position = "bottom", axis.title.x = element_blank(), 
        axis.title.y = element_blank(), plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(labels = function(x) stringr::str_pad(x, width = 2, pad = "0")) + 
  scale_y_continuous(label = unit_format(unit = "K", scale = 1/1000, sep = "")) + 
  scale_color_manual(values = c("royalblue", "red", "darkgrey")) + 
  ggtitle("Average NFL attendance (per game) 2000-2019")