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

call.data <- read.csv(file = "./data/my_call_data_80_100_10_pct.csv", header = TRUE)

# weekend data

my.call.data <- subset(call.data, date > 20080300) #change date
my.call.data <- subset(call.data, date < 20080308)

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

global.coeficient.1 <- transitivity(g, type="global")
local.coeficient.1 <- transitivity(g, type="local")