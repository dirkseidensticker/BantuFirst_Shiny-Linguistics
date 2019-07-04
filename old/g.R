library(sf)
library(ggplot2)
library(rnaturalearth)
# get rivers
rivers10 <- ne_download(scale = 10, type = 'rivers_lake_centerlines', category = 'physical')
coast10 <- ne_download(scale = 10, type = 'coastline', category = 'physical')






g <- ggplot() + 
  geom_path(data = coast10,
            aes(long, lat, group = group), size = .5, color = '#44afe3') +
  geom_path(data = rivers10,
            aes(long, lat, group = group), size = .5, color = '#44afe3') +
  coord_sf(xlim = c(floor(min(sara$long)), ceiling(max(sara$long))),
           ylim = c(floor(min(sara$lat)), ceiling(max(sara$lat))))

g +
  geom_point(data = sara, aes(x = long, y = lat, color = tree.V1)) + 
  ggtitle("Level 1")
g +
  geom_point(data = sara, aes(x = long, y = lat, color = tree.V2)) + 
  ggtitle("Level 2")
g +
  geom_point(data = sara, aes(x = long, y = lat, color = tree.V3)) + 
  ggtitle("Level 3")
g +
  geom_point(data = sara, aes(x = long, y = lat, color = tree.V4)) + 
  ggtitle("Level 4")
g +
  geom_point(data = sara, aes(x = long, y = lat, color = tree.V5)) + 
  ggtitle("Level 5")
g +
  geom_point(data = sara, aes(x = long, y = lat, color = tree.V6)) + 
  ggtitle("Level 6")
g +
  geom_point(data = sara, aes(x = long, y = lat, color = tree.V7)) + 
  ggtitle("Level 7")


