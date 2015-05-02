rm(list = ls())

## Load the required packages, assuming they have been installed
library(rmongodb)
library(ggplot2)
library(scales)

## Source the useful functions
source("./code/util/fivethirtyeight_theme.R") # fancy-looking theme for the plot

## Login credentials
host <- "heinz-tjle.heinz.cmu.edu"
username <- "student"
password <- "helloWorld"
db <- "admin"

## Connect to MongoDB remote server
mongo <- mongo.create(host = host, db = db, username = username, password = password)
## Check if we are successfully connected
mongo.is.connected(mongo)

## The database we're working with is 'admin' and the collection is 'cellular'
collection <- "cellular"
namespace <- paste(db, collection, sep=".")

## WARNING: This may take some time
## First, group by date and count the frequencies
pipe_1 <- mongo.bson.from.JSON(
  '{"$group":
{"_id": "$date", "count": {"$sum": 1}}
  }'
)
## Then, sort by frequency in descending order
pipe_2 <- mongo.bson.from.JSON(
  '{"$sort": {"count": -1}}'
)
## Concatenate the pipeline
pipeline <- list(pipe_1, pipe_2)
## Run the aggregation pipeline
dnum.distr <- mongo.aggregation(mongo, namespace, pipeline)

## Reshape the data to fit into an R data frame
ldnum.distr <- mongo.bson.value(dnum.distr, "result")
mdnum.distr <- sapply(ldnum.distr, function(x) return(c(toString(x["_id"]),
                                                      as.numeric(x["count"]))))
mdnum.distr <- t(mdnum.distr) # transpose the matrix
date.num <- as.numeric(mdnum.distr[, 2]) # convert num into numeric
ddnum.distr <- as.data.frame(mdnum.distr) # convert matrix into data frame
colnames(ddnum.distr) <- c("date", "num") # name the columns
ddnum.distr$num <- date.num

write.csv(ddnum.distr, file="./data/my_date_distr.csv", row.names=FALSE)

## Release the resources attached to cursor on both client and server
mongo.cursor.destroy(cursor)
## Close the connection
mongo.disconnect(mongo)
mongo.destroy(mongo)

ddnum.distr <- read.csv(file="./data/my_date_distr.csv")

## Sort the data frame by frequency in decreasing order
ddnum.distr <- ddnum.distr[order(ddnum.distr$date), ]

## Plot the histogram of the Date Data
(ggplot(ddnum.distr, aes(num)) + geom_histogram(binwidth=2, fill="#c0392b", alpha=0.75) +
   fivethirtyeight_theme() + 
   labs(title="Distribution of Call On Dates",
        x="Date", y="Data Num") + scale_x_continuous(labels=comma) +
   scale_y_continuous(labels=comma) + geom_hline(yintercept=0, size=0.4, color="black"))

## Save the plot to disk
ggsave(file="./figures/call_freq_date.png", width=4, height=3)
