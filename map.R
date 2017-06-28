require('dplyr')
require('ggplot2')
require('rgdal')
require('readr')


# load in our shapefile and convert to a data frame

counties <- readOGR(dsn = 'shp', layer = 'counties', verbose = FALSE) %>%
  fortify(region = 'AFFGEOID')


# load in our external data and do some calculcations

languages <- read_csv('languages.csv') %>%
  mutate(share = count/pop) %>%
  group_by(language) %>%
  mutate(sds = (share - mean(share))/sd(share)) %>%
  select(Id, Geography, share, sds)


# join the map data and external data

mapData <- left_join(counties, languages, by = c('id' = 'Id'))


# plot the joined data

map <- ggplot(mapData, aes(x = long, y = lat, group = group, fill = sds >= 1.5)) +
  geom_polygon(color = 'NA') +
  coord_map("albers", lat0 = 29.5, lat1 = 45.5) +
  theme_void() +
  scale_fill_manual(values=c('#cccccc', '#ff0000')) +
  facet_wrap(~language, ncol = 5)

print(map)