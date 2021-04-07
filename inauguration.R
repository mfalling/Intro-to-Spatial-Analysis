
# Library -----------------------------------------------------------------

library(dplyr)


# About -------------------------------------------------------------------

# Exploring .shp files and spatial analysis in R.
# Layering ridership data from Obama and Trump inauguration days with
# the geo-coordinates from the .shp file

# Load Data ---------------------------------------------------------------

# Data from https://data.world/transportation/wmata-ridership-data
obama <- read.csv("data/inaugurations/2009-Inauguration-Records-Raw.csv")
trump <- read.csv("data/inaugurations/2017-Inauguration-Records-Raw.csv")
stations <- st_read("data/stations/stations.shp")


# Variable Description

# AM Peak: Station Opening to 9:30 AM.
# Mid-day: 9:30 AM – 3:00 PM.
# PM Peak: 3:00 PM- 7:00 PM.
# Evening: 7:00 PM to Station Closing.
# The “entry” number is what is used for official ridership stats.


# Cleaning ----------------------------------------------------------------

# Prep the time-of-day columns for conversion to numeric type
times <- colnames(obama)[4:7]
obama[times] <- apply(obama[times], 2, function(x){gsub(",","",x)})

# Clean
obama <- obama %>%
  select(!c(equivalent_date_last_yr, sum)) %>%
  filter(type == "Entry") %>%
  mutate_at(times, as.numeric)

# Prep name columns for match
obama$stations_name <- gsub("/|-", " ", obama$stations_name)
stations$name <- gsub("/|-", " ", stations$name)

# Because names don't match exactly, use fuzzy matching and store the comparisons.
matches <- NULL
for (i in 1:nrow(obama)){
  index <- agrep(obama$stations_name[i], stations$name, ignore.case = TRUE, max.distance = .25)
  if (length(index) > 0){
    match <- data.frame(obama = obama$stations_name[i], stations = stations$name[index])
  }
  if (length(index) == 0){
    match <- data.frame(obama = obama$stations_name[i], stations = "NO MATCH")
  }
  matches <- rbind(matches, match)
}

# Find where agrep returned multiple matches for the same pattern
ambiguousMatches <- matches %>%
  group_by(obama) %>%
  tally() %>%
  filter(n > 1)

# Find the ambiguous matches in the matches df.
ambiguousAudit <- NULL
for (i in 1:nrow(ambiguousMatches)){
  ambiguousMatch <- matches[which(matches$obama == ambiguousMatches$obama[i]), ]
  ambiguousAudit <- rbind(ambiguousAudit, ambiguousMatch)
}
ambiguousAudit

# Manually review and update matches using the row numbers found in ambiguousAudit
matches <- matches[-c(2, 25, 29, 32, 34, 35, 49, 57, 69, 84, 97),]
matches

# Find all the missing matches.
missingMatches <- matches %>%
  group_by(obama) %>%
  filter(stations == "NO MATCH")
missingMatches

matches$stations[79] <- stations$name[9]

# Drop stations that don't have matches
matches <- matches %>%
  filter(stations != "NO MATCH")
matches

# Rename back to original, for ease of joins.
colnames(matches)[2] <- "name"
colnames(matches)[1] <- "stations_name"

# Join the data
stations <- inner_join(matches, stations, by = "name")
obama <- inner_join(matches, obama, by = "stations_name")
fullset <- inner_join(stations, obama)

# Drop unnecessary columns
fullset <- fullset %>%
  select(-c("type", "name", "marker.sym"))

head(fullset)
