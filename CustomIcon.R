source("data.R")

# ------

library(ggplot2)


icon.col <- data.frame(unique(d$branch), rainbow(length(unique(d$branch))))
names(icon.col) <- c("branch", "col")
icon.col$col <- as.character(icon.col$col)
icon.col$col <- substr(icon.col$col,1,nchar(icon.col$col)-2)

d <- merge(x = d, y = icon.col, by = "branch")
for (i in 1:nrow(d)) {
  ggplot() + 
    geom_text(data = d[i,], 
              aes(x = 0, y = 0, 
                  label = guthrieCode),
              colour = d[i,"col"],
              size = 6) + 
    theme_void() + 
    theme(legend.position="none")
  ggsave(paste0("icons/", rownames(d[i,]),".png"), width = 20, height = 5, units = "mm")
}
