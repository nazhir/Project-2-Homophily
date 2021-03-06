rm(list = ls())

## Load the required packages, assuming they have been installed
library(rmongodb)
library(ggplot2)
library(ggmap) # for plotting maps
library(igraph) # for plotting graphs
library(popgraph) # for plotting population graphs
library(scales) # for plot formatting 
library(plyr) # for data manipulation

## Source the useful functions
source("./code/util/fivethirtyeight_theme.R") # fancy-looking theme for the plot
source("./code/util/getWeightedEdges.R") # create weighted call graph
source("./code/util/aggregateLocationByDate.R") # for call data aggregation by date
source("./code/util/aggregateLocationByHour.R") # for call data aggregation by hour
source("./code/util/aggregateLocationByPeriod.R") # for call data aggregation by period

## Read the cell towers spatial coordinates
cell.towers <- read.csv(file="./data/cell_coord.csv", stringsAsFactors=FALSE)
## Convert from string to numeric format
cell.towers$Longitude <- as.numeric(cell.towers$Longitude)
cell.towers$Latitude <- as.numeric(cell.towers$Latitude)

## Load the cell_id rowIndex lookup table (from the previous
## tutorial: http://vietletruc.com/wp-content/uploads/2015/03/cell_locations.html)
load("./data/cell_id_rowIndex_mapping.RData")

## Load the previously retrieved call data
## NB: This is the call data frame retrieved from the lines below (67-155).
## Because it has taken a significant retrieval time, I saved it as an offline file
## and load it whenever I want to use it. If you want to use this data frame, skip lines
## 67 to 155 to save time. Feel free to experiment with other params and get a diff dataset.
call.data <- read.csv(file="./data/my_call_data_80_100_10_pct.csv")

## Match each cell_id with its corresponding rowIndex of the coordinate table
progress.bar <- create_progress_bar("text")
progress.bar$init(nrow(call.data))
rowIndices <- vector() # a vector of row indices
nMatches <- 0 # count the number of matches
for(i in 1:nrow(call.data)) {
  ## Have to convert numeric to string in order to do lookup
  cell_id <- toString(call.data$cell_id[i])
  if(nchar(cell_id) > 0) {
    ## Look up the (loaded) table
    rowIndex <- cell_id.coord.rowIndex[[cell_id]]
    if(is.null(rowIndex)) { # if matched
      rowIndices <- c(rowIndices, NA)
    } else { # if unmatched
      rowIndices <- c(rowIndices, rowIndex)
      nMatches <- nMatches + 1
    }
  } else {
    rowIndices <- c(rowIndices, NA)
  }
  
  progress.bar$step()
}
## Calculate the matched percentage
match.pct <- round(nMatches / nrow(call.data) * 100, 2)
print(paste("Percent matched locations =", match.pct))

#Saturday

## creating call bubble plot start

## Reduce call data to those that can be plotted on a map
my.call.data <- call.data[!is.na(rowIndices), ]
## Further reduce to include a certain date range only. Why?
# (table(my.call.data$date))

my.call.data <- subset(my.call.data, date > 20080300) #change date
my.call.data <- subset(my.call.data, date < 20080308)

hour <- vector()
period <- vector()
progress.bar <- create_progress_bar("text")
progress.bar$init(nrow(my.call.data))
for(i in 1:nrow(my.call.data)) {
  timestamp <- toString(my.call.data$time[i])
  timestamp <- strsplit(timestamp, ":")[[1]]
  hour[i] <- timestamp[1]
  
  if(as.numeric(hour[i])>17) {
    period[i] <- 4
  } else if (as.numeric(hour[i])>11) {
    period[i] <- 3
  } else if (as.numeric(hour[i])>5) {
    period[i] <- 2
  } else {
    period[i] <- 1
  }
  
  progress.bar$step()
}
my.call.data$hour <- hour
my.call.data$period <- period

## Aggregate all the call records by date and find the locations of the cell towers
## and the frequency of the calls made from each.
#aggregate.location <- aggregateLocationByDate(my.call.data,cell_id.coord.rowIndex, cell.towers)
aggregate.location <- aggregateLocationByHour(my.call.data,cell_id.coord.rowIndex, cell.towers)

hourType <- vector()
progress.bar <- create_progress_bar("text")
progress.bar$init(nrow(aggregate.location))
for(i in 1:nrow(aggregate.location)) {
  temp <- aggregate.location$hour[i]
  
  if(temp == "00") {
    hourType[i] <- "01. 00.00 AM - 00.59 AM"
  } else if (temp == "01") {
    hourType[i] <- "02. 01.00 AM - 01.59 AM"
  } else if (temp == "02") {
    hourType[i] <- "03. 02.00 AM - 02.59 AM"
  } else if (temp == "03") {
    hourType[i] <- "04. 03.00 AM - 03.59 AM"
  } else if (temp == "04") {
    hourType[i] <- "05. 04.00 AM - 04.59 AM"
  } else if (temp == "05") {
    hourType[i] <- "06. 05.00 AM - 05.59 AM"
  } else if (temp == "06") {
    hourType[i] <- "07. 06.00 AM - 06.59 AM"
  } else if (temp == "07") {
    hourType[i] <- "08. 07.00 AM - 07.59 AM"
  } else if (temp == "08") {
    hourType[i] <- "09. 08.00 AM - 08.59 AM"
  } else if (temp == "09") {
    hourType[i] <- "10. 09.00 AM - 09.59 AM"
  } else if (temp == "10") {
    hourType[i] <- "11. 10.00 AM - 10.59 AM"
  } else if (temp == "11") {
    hourType[i] <- "12. 11.00 AM - 11.59 AM"
  } else if (temp == "12") {
    hourType[i] <- "13. 12.00 PM - 12.59 PM"
  } else if (temp == "13") {
    hourType[i] <- "14. 01.00 PM - 01.59 PM"
  } else if (temp == "14") {
    hourType[i] <- "15. 02.00 PM - 02.59 PM"
  } else if (temp == "15") {
    hourType[i] <- "16. 03.00 PM - 03.59 PM"
  } else if (temp == "16") {
    hourType[i] <- "17. 04.00 PM - 04.59 PM"
  } else if (temp == "17") {
    hourType[i] <- "18. 05.00 PM - 05.59 PM"
  } else if (temp == "18") {
    hourType[i] <- "19. 06.00 PM - 06.59 PM"
  } else if (temp == "19") {
    hourType[i] <- "20. 07.00 PM - 07.59 PM"
  } else if (temp == "20") {
    hourType[i] <- "21. 08.00 PM - 08.59 PM"
  } else if (temp == "21") {
    hourType[i] <- "22. 09.00 PM - 09.59 PM"
  } else if (temp == "22") {
    hourType[i] <- "23. 10.00 PM - 10.59 PM"
  } else if (temp == "23") {
    hourType[i] <- "24. 11.00 PM - 11.59 PM"
  } else {
    hourType[i] <- "02. 24 PM"
  }
  
  progress.bar$step()
}
aggregate.location$hourType <- hourType

## Create a spatial bubble chart
## First, need a location at the center of all cell towers in my call data
## Calculate the mean longitude
all.longitude <- names(table(aggregate.location$longitude))
all.longitude <- as.numeric(all.longitude)
mean.longitude <- mean(all.longitude)

## Calculate the mean latitude
all.latitude <- names(table(aggregate.location$latitude))
all.latitude <- as.numeric(all.latitude)
mean.latitude <- mean(all.latitude)

## Retrieved a map centered at the given location
location <- c(mean.longitude, mean.latitude)
location.map <- get_map(location, maptype = "terrain", zoom = 10, scale=2)
location.map <- ggmap(location.map, extent = 'device', legend = 'none')
## Add the "bubbles" to the map, and size them by frequency.
## Here we choose shape 21 (circle) and fill it red. For a full list of shapes,
## see http://sape.inf.usi.ch/quick-reference/ggplot2/shape
location.map <- location.map + geom_point(data = aggregate.location,
                                          aes(x = longitude, y = latitude, size = freq),
                                          fill="red", alpha=0.80, shape=21)
## Remove any legends from the plot
location.map <- location.map + guides(fill=FALSE, alpha=FALSE, size=FALSE)
## Add a title to the plot
location.map <- location.map + ggtitle("Hourly Distribution of Call Events for 80 - 100")
## Add a fancy theme to it (this step is optional)
location.map <- location.map + fivethirtyeight_theme()
## Split the plot into tiles (or facets), one for each hour
location.map <- location.map + facet_wrap(~hourType)
## Display the plot (or else it won't show)
print(location.map)

## Save the plot on disk
ggsave(filename="./figures/my_call_bubbles_hourly_80_100.png", width=10, height=10) #change date

## creating call bubble plot end