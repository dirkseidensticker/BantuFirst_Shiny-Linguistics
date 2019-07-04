library(stringr)
library(colorspace)
library(RColorBrewer)

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

klc$branch.legend <- paste0("KLC/", klc$branch)

# KLC colors palette:
#klc.col <- as.data.frame(unique(klc$branch.legend))
#colnames(klc.col) <- "branch.legend"
#display.brewer.pal(n = nrow(klc.col), name = 'GnBu')
#klc.col$branch.col <- brewer.pal(n = nrow(klc.col), name = 'GnBu')
#klc.col$branch.col <- darken(klc.col$branch.col, 0.2)

#klc$id <- as.numeric(rownames(klc))
#klc <- merge(x = klc, y = klc.col, by = "branch.legend")
#klc <- klc[order(klc$id),]
#rownames(klc) <- klc$id

# Neuer Datensatz von Sara
# ------------------------
sara <- as.data.frame(read_excel("geocoordinates DS_update 20190620.xlsx")) %>%
  dplyr::transmute(
    branch = .data[["Path"]],
    guthrieCode = .data[["Guthrie (-inspired) Code"]],
    variety = .data[["Variety"]],
    long = .data[["longitude (geonames.org)"]],
    lat = .data[["latitude (geonames.org)"]],
    source = .data[["Sources"]]
  ) %>% dplyr::filter(!is.na(long))

sara$tree <- as.data.frame(str_split_fixed(sara$branch, "_", max(str_count(sara$branch, "_"))+1))
sara <- do.call(data.frame, sara)

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

# see https://regex101.com/ >> "^.*\_"
sara$branch.legend <- sub('^.*_', "", sara$branch)

# colors for saras dataset
#sara.col <- as.data.frame(unique(sara$branch.legend))
#colnames(sara.col) <- "branch.legend"
##display.brewer.pal(n = nrow(sara.col), name = 'OrRd')
#sara.col$branch.col <- brewer.pal(n = nrow(sara.col), name = 'OrRd')
#sara.col$branch.col <- darken(sara.col$branch.col, 0.2)

#sara$id <- as.numeric(rownames(sara))
#sara <- merge(x = sara, y = sara.col, by = "branch.legend")
#sara <- sara[order(sara$id),]
#rownames(sara) <- sara$id

# Concatenate the two dataframes 
# ------------------------------
d <- rbind.fill(klc, sara)

# Labels for Legend for Phylogenetic Classification / Branch
# ----------------------------------------------------------
# d$branch.legend[d$branch.legend == "KLC/NA"] <- "WCB outside KLC"

# RowNr as id col
d$id <- as.numeric(rownames(d))