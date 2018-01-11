# Territory

```

# An analysis of territories in Risk

library(readr)
library(geomnet)
library(ggplot2)
country.edges <- read.csv('Country_Edges.csv', na.strings = '')

edges <- country.edges[c("T1", "T2")]
vertices <- unique(country.edges[c("T1", "C1")])

Risk.net <- fortify(as.edgedf(edges), vertices)

set.seed(10052016)
ggplot(data = Risk.net, aes(from_id = from_id, to_id = to_id)) +
  geom_net(aes(colour = C1), layout.alg = "fruchtermanreingold", 
           size = 2, labelon = TRUE, vjust = -0.6, ecolour = "grey60",
           directed =FALSE, fontsize = 3, ealpha = 0.5) +
  scale_colour_manual(values = c("orange", "red", "blue", "black", "green", "purple")) +
  xlim(c(-0.05, 1.05)) +
  theme_net() +
  theme(legend.position = "bottom")

country.edges$cross.continent <- 1

country.edges$cross.continent <- replace(country.edges$cross.continent, which(country.edges$C1==country.edges$C2), 0)

library(dplyr)
territory.entry <- country.edges %>% 
  group_by(T1) %>%
  summarise(cross.cont = sum(cross.continent), no.edges = n(), no.cont = n_distinct(C2))

continent.entry <- country.edges %>% 
  group_by(C1) %>%
  summarise(cross.cont = sum(cross.continent), no.edges = n(), no.cont = n_distinct(C2))

territory.entry$entry.ratio <- territory.entry$cross.cont / territory.entry$no.edges
continent.entry$entry.ratio <- continent.entry$cross.cont / continent.entry$no.edges

territory.entry$no.cont <- territory.entry$no.cont - 1
continent.entry$no.cont <- continent.entry$no.cont - 1

```
