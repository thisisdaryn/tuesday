library(ggplot2)
library(dplyr)

sf_tall <- filter(sf_trees, dbh < 1000, dbh >= 100)  %>% arrange(desc(dbh)) %>%
  mutate(rank = 1:n())

### there should be a neater way to make this new dataframe. Basically, I want 
### three copies of each row to add the number of the pane I want the plot to show 
### up in
sf1 <- sf_tall
sf2 <- sf_tall
sf3 <- sf_tall

sf1$pane <- 1
sf2$pane <- 2
sf3$pane <- 3

sf_tall <- bind_rows(sf1, sf2, sf3)

sf_tall <- mutate(sf_tall, 
                   y = if_else(pane %in% c(1,2), latitude, dbh),
                   x = if_else(pane %in% c(1,3), longitude, dbh),
                  xs1 = if_else(pane %in% c(1,3), x, 0),
                  xs2 = if_else(pane %in% c(1,3), x, dbh),
                  ys1 = if_else(pane %in% c(1,2), y, 0),
                  ys2 = if_else(pane %in% c(1,2), y, dbh),
                  label = if_else(pane == 1, "lat vs lon", 
                                  if_else(pane == 2, "lat vs height", 
                                          "height vs lon")))

sf_tall$label <- factor(sf_tall$label, levels = c("lat vs lon", "lat vs height", 
                                                  "height vs lon"))


ggplot(data = sf_tall) + 
  geom_point(aes(x = x, y = y), colour = "darkgreen", shape = 21) + 
  geom_segment(aes(x = xs1, y = ys1, xend = xs2, yend = ys2), 
               colour = "sienna") + 
  facet_wrap(~label, ncol = 2, scale = "free") + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Tallest trees in San Francisco") 

