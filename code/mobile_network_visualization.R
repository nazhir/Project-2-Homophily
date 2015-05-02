## Author: Truc Viet 'Joe' Le at tjle@andrew.cmu.edu

## This script demonstrates basic the analysis of network structure: degree distributions.
## It first constructs an igraph object from the input CSV file. It then fits the power
## law to 3 kinds of degree distribution: all degree, indegree and outdegree for the 
## constructed directed network. Power law is fitted using the helper function 'plfit'.
## For each distribution, an alpha exponent is computed together with the Kolmogorov–Smirnov 
## (KS) test for significance of alpha. If alpha < 0.05, we say that it is significant
## according to the test. Finally, plots of the power-law fits are produced: histogram and
## cumulative distribution plots on log-log scale with the fitted straight lines.

rm(list = ls()) # clear the workspace

library(igraph)
library(stats4)
library(splines)

## Load the useful functions
source("./code/sna/plfit.R")
source("./code/sna/getGraphObj.R")
source("./code/sna/plotNetwork.R")

call.data <- read.csv(file = "./data/my_call_data_100_300_1500.csv", header = TRUE)

# weekend data

my.call.data <- subset(call.data, date > 20080300) #change date
my.call.data <- subset(call.data, date < 20080303)

## Select the callers that are also callees
call_ids <- intersect(my.call.data$caller_id, my.call.data$callee_id)
subset.call.data <- subset(my.call.data, caller_id %in% call_ids)
subset.call.data <- subset(subset.call.data, callee_id %in% call_ids)

## Create a data frame that stores the relationships (i.e., edges)
## among the agents. Strength of the relationship is represented by call frequency.
edges <- data.frame(callee_id = subset.call.data$callee_id,caller_id = subset.call.data$caller_id)
weightedEdges <- data.frame(table(edges))

weightedEdges <- subset(weightedEdges, Freq > 1) ## choose k frequency

## First remove the 'freq' column from the matrix
weightedEdges.matrix <- as.matrix(weightedEdges[, -3])
## The graph has 8,908 nodes (or unique users)
g <- graph.edgelist(weightedEdges.matrix, directed = TRUE)
E(g)$weight <- weightedEdges$Freq # add weights to the edges

## Configure the graph for visualization
V(g)$size <- degree(g, mode = "all")

# ## Exclude those nodes that are at the periphery of the network
# theta <- 5 # threshold
# bad.nodes <- V(g)[degree(g) < theta] # low-degree nodes
# f <- delete.vertices(g, bad.nodes) # f is the new network

## Run community detection and color those communities
## Fastgreedy community finding algorithm (greedy optimization of modularity)
fc <- fastgreedy.community(as.undirected(g))
# print("Fast Greedy community sizes:")
# print(sizes(fc))

## Sample random colors for the communities
fc.colors <- sample(colors(), length(fc))
## Color the nodes by the community
for(i in 1:length(fc.colors)) {
  V(g)[membership(fc)==i]$color <- fc.colors[i]
}

## Size the vertices according to their outdegrees
V(g)$size <- degree(g, mode="all") / 2

## Finally, plot and save the network
filename <- "./figures/mobile_network_igraph_weekend.pdf"
mainStr <- "Mobile Data Network Visualization on Weekend"
plotNetwork(g, mainStr, filename, width=15, height=15)

temp.df <- as.data.frame(membership(fc))
colnames(temp.df) <- c("group")
call_num <- rownames(temp.df)

label.df <- data.frame(call_num = call_num, group = temp.df$group )

group.df <- data.frame(table(temp.df))
colnames(group.df) <- c("group","freq")

group.df <- subset(group.df, freq > 2) ## choose k frequency

subset.label.df <- subset(label.df, group %in% group.df$group)

write.csv(subset.label.df, file="./data/my_social_weekend.csv", row.names=FALSE)

#weekdays data

my.call.data <- subset(call.data, date > 20080302) #change date
my.call.data <- subset(call.data, date < 20080309)

## Select the callers that are also callees
call_ids <- intersect(my.call.data$caller_id, my.call.data$callee_id)
subset.call.data <- subset(my.call.data, caller_id %in% call_ids)
subset.call.data <- subset(subset.call.data, callee_id %in% call_ids)

## Create a data frame that stores the relationships (i.e., edges)
## among the agents. Strength of the relationship is represented by call frequency.
edges <- data.frame(callee_id = subset.call.data$callee_id,caller_id = subset.call.data$caller_id)
weightedEdges <- data.frame(table(edges))

weightedEdges <- subset(weightedEdges, Freq > 1) ## choose k frequency

## First remove the 'freq' column from the matrix
weightedEdges.matrix <- as.matrix(weightedEdges[, -3])
## The graph has 8,908 nodes (or unique users)
g <- graph.edgelist(weightedEdges.matrix, directed = TRUE)
E(g)$weight <- weightedEdges$Freq # add weights to the edges

## Configure the graph for visualization
V(g)$size <- degree(g, mode = "all")

# ## Exclude those nodes that are at the periphery of the network
# theta <- 5 # threshold
# bad.nodes <- V(g)[degree(g) < theta] # low-degree nodes
# f <- delete.vertices(g, bad.nodes) # f is the new network

## Run community detection and color those communities
## Fastgreedy community finding algorithm (greedy optimization of modularity)
fc <- fastgreedy.community(as.undirected(g))
# print("Fast Greedy community sizes:")
# print(sizes(fc))

## Sample random colors for the communities
fc.colors <- sample(colors(), length(fc))
## Color the nodes by the community
for(i in 1:length(fc.colors)) {
  V(g)[membership(fc)==i]$color <- fc.colors[i]
}

## Size the vertices according to their outdegrees
V(g)$size <- degree(g, mode="all") / 2

## Finally, plot and save the network
filename <- "./figures/mobile_network_igraph_weekdays.pdf"
mainStr <- "Mobile Data Network Visualization on Weekdays"
plotNetwork(g, mainStr, filename, width=15, height=15)

temp.df <- as.data.frame(membership(fc))
colnames(temp.df) <- c("group")
call_num <- rownames(temp.df)

label.df <- data.frame(call_num = call_num, group = temp.df$group )

group.df <- data.frame(table(temp.df))
colnames(group.df) <- c("group","freq")

group.df <- subset(group.df, freq > 2) ## choose k frequency

subset.label.df <- subset(label.df, group %in% group.df$group)

write.csv(subset.label.df, file="./data/my_social_weekdays.csv", row.names=FALSE)


