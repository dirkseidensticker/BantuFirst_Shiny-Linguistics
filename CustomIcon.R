# Alter KLC Datensatz
# -------------------
klc <- as.data.frame(read_excel("Map PhD SB_KB.xlsx")) %>%
  dplyr::transmute(
    branch = .data[["KLC branch"]], 
    guthrieCode = .data[["GUTHRIE"]],
    variety = .data[["Variety"]],
    code = .data[["Code"]],
    long = .data[["Longitude"]],
    lat = .data[["Latitude"]],
    source = .data[["Source"]],
    place = .data[["Place"]]
  ) %>% dplyr::mutate(
    long = round(as.numeric(long), 2),
    lat = round(as.numeric(lat), 2)
  ) %>% dplyr::filter(!is.na(guthrieCode))

klc$guthrie <- paste(substr(klc$guthrieCode, 1, 1), 
                     10 * floor(as.numeric(substr(gsub("[^[:digit:]]", 
                                                       "", 
                                                       klc$guthrieCode), 
                                                  1, 2))/10), 
                     sep = '')

# remove prefixes
klc$variety <- gsub("Ki|Di|Yi|I|Ci", "", klc$variety)
klc$variety <- paste(toupper(substr(klc$variety, 1, 1)), substr(klc$variety, 2, nchar(klc$variety)), sep="")

# Neuer Datensatz von Sara
# ------------------------
sara <- as.data.frame(read_excel("geocoordinates 20181008_for website.xlsx")) %>%
  dplyr::transmute(
    guthrieCode = .data[["Guthrie (-inspired) Code"]],
    variety = .data[["Variety"]],
    long = .data[["longitude (geonames.org)"]],
    lat = .data[["latitude (geonames.org)"]],
    source = .data[["Sources"]]
  ) %>% dplyr::filter(!is.na(long))

# create colum with unique colors for individual variety
# cut everything after the 3rd position to get somewhat of a reasonable colormap
# see http://r.789695.n4.nabble.com/Ceiling-to-the-nearest-ten-tp844951p844952.html
sara$guthrie <- paste(substr(sara$guthrieCode, 1, 1), 
                      10 * floor(as.numeric(substr(gsub("[^[:digit:]]", "", sara$guthrieCode), 1, 2))/10), 
                      sep = '')

# für alle kleinen Buchstaben gsub("[^[:lower:]]", "", d$Guthrie...inspired..Code)
sara$guthrieCodeLang <- gsub("[^xyz]", 
                             "", 
                             sara$guthrieCode)

# Alle großen Buchstaben
sara$guthrieDialect <- substring(gsub("[^[:upper:]]", 
                                      "", 
                                      sara$guthrieCode), 2)

# Concatenate the two dataframes 
# ------------------------------
d <- rbind.fill(klc, sara)

# Labels for Legend for Phylogenetic Classification / Branch
# ----------------------------------------------------------
d$branch.legend <- paste0("KLC/", unique(d$branch))
d$branch.legend[d$branch.legend == "KLC/NA"] <- "WCB outside KLC"


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
              size = 30) + 
    theme_void() + 
    theme(legend.position="none")
  ggsave(paste0("icons/", rownames(d[i,]),".png"), width = 4, height = 1)
}
