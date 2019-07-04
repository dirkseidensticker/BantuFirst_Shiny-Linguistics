source("data.R")

# ------

library(colorspace)
library(ggplot2)
library(plyr)
library(viridis)
library(RColorBrewer)

b.cols <- read.csv("cols_branch.csv", encoding = "UTF8")

d <- merge(x = d, y = b.cols, by = "branch.legend")

#icon.col <- data.frame(unique(d$branch), viridis(length(unique(d$branch.legend))))
#names(icon.col) <- c("branch", "col")
#icon.col$col <- as.character(icon.col$col)
#icon.col$col <- substr(icon.col$col,1,nchar(icon.col$col)-2)

# tree different pallets for KLC &  & East Yans
#klc <- icon.col[1:7,]
#display.brewer.pal(n = nrow(klc), name = 'GnBu')
#klc$col <- brewer.pal(n = nrow(klc), name = 'GnBu')
#klc$col <- darken(klc$col, 0.2)

#kwng <- icon.col[c(8:13,15:16),]
#display.brewer.pal(n = nrow(kwng), name = 'OrRd')
#kwng$col <- rev(brewer.pal(n = nrow(kwng), name = 'OrRd'))
#kwng$col <- darken(kwng$col, 0.2)

#kakw <- icon.col[14,]
#kakw$col <- "#efef34"

# icon.col <- rbind(klc, kwng, kakw)

# d <- merge(x = d, y = icon.col, by = "branch")
d <- d[order(d$id),]
rownames(d) <- d$id

for (i in 1:nrow(d)) {
  ggplot() + 
    geom_label(data = d[i,], 
              aes(x = 0, y = 0, 
                  label = guthrieCode),
              fill = d[i,"branch.col"],
              color = "white",
              size = 5) + 
    theme_void() + 
    theme(legend.position="none")
  ggsave(paste0("icons_branch/", rownames(d[i,]),".png"), width = 20, height = 7, units = "mm", bg = "transparent")
}

# GUTHRIE CLASSIFICATION

g.cols <- data.frame(unique(d$guthrie), viridis(length(unique(d$guthrie))))
names(g.cols) <- c("guthrie", "guthrie.col")
d <- merge(x = d, y = g.cols, by = "guthrie")
d <- d[order(d$id),]
rownames(d) <- d$id

for (i in 1:nrow(d)) {
  ggplot() + 
    geom_label(data = d[i,], 
               aes(x = 0, y = 0, 
                   label = guthrieCode),
               fill = d[i,"guthrie.col"],
               color = "white",
               size = 5) + 
    theme_void() + 
    theme(legend.position="none")
  ggsave(paste0("icons_guthrie/", rownames(d[i,]),".png"), width = 20, height = 7, units = "mm", bg = "transparent")
}

