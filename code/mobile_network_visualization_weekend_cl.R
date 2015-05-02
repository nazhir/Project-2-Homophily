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

call.data <- read.csv(file="./data/my_call_data_80_100_10_pct.csv", header=TRUE)
call.cluster <- read.csv(file = "./data/my_weekend_caller_cluster.csv", header = TRUE)

# create social graph for cluster #1

subset.call.cluster <- subset(call.cluster, cluster %in% 1)

subset.caller.data <- subset(call.data, caller_id %in% subset.call.cluster$call_num)
subset.callee.data <- subset(call.data, callee_id %in% subset.call.cluster$call_num)

my.cluster.call <- rbind(subset.caller.data,subset.callee.data)

my.call.data <- subset(my.cluster.call, date > 20080300) #change date
my.call.data <- subset(my.cluster.call, date < 20080303)

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
print(sizes(fc))

## Sample random colors for the communities
fc.colors <- sample(colors(), length(fc))
## Color the nodes by the community
for(i in 1:length(fc.colors)) {
  V(g)[membership(fc)==i]$color <- fc.colors[i]
}

## Size the vertices according to their outdegrees
V(g)$size <- degree(g, mode="all") / 2

## Finally, plot and save the network
filename <- "./figures/mobile_network_igraph_weekend_cluster1.pdf"
mainStr <- "Mobile Data Network Visualization on Weekend Cluster #1"
plotNetwork(g, mainStr, filename, width=15, height=15)

## Get Coeficient Clustering

my.cluster.call <- rbind(subset.caller.data,subset.callee.data)

my.call.data <- subset(my.cluster.call, date > 20080300) #change date
my.call.data <- subset(my.cluster.call, date < 20080303)

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

## Global clustering coefficient of the network
print("Global clustering coefficient of g:")
print(transitivity(g, type="global"))

## Network structure -- degree distributions: all degrees, indegree, and outdegree
degrees <- degree(g, mode = "all")
degrees <- subset(degrees, degrees > 0) # remove all zero degrees
indegrees <- degree(g, mode = "in")
indegrees <- subset(indegrees, indegrees > 0)
outdegrees <- degree(g, mode = "out")
outdegrees <- subset(outdegrees, outdegrees > 0)

## Fit the power law to 3 kinds of degree distribution
pl <- plfit(degrees)
# pl.in <- plfit(indegrees)
# pl.out <- plfit(outdegrees)
print("Overall degree distribution:")
print(pl) # test if it is power-lawed and print its alpha exponent
# print("Indegree distribution:")
# print(pl.in)
# print("Outdegree distribution:")
# print(pl.out)

## Plot the cumulative empirical distribution
cumy <- c() # empty, expandable vector
y <- tabulate(degrees)
x <- 1:length(y)
for(i in 1:length(x)) {
  cumy[i] <- sum(y[i:length(x)]) / sum(y)
}
# ## For indegrees
# cumy.in <- c()
# y.in <- tabulate(indegrees)
# x.in <- 1:length(y.in)
# for(i in 1:length(x.in)) {
#   cumy.in[i] <- sum(y.in[i:length(x.in)]) / sum(y.in)
# }
# ## For outdegrees
# cumy.out <- c()
# y.out <- tabulate(outdegrees)
# x.out <- 1:length(y.out)
# for(i in 1:length(x.out)) {
#   cumy.out[i] <- sum(y.out[i:length(x.out)]) / sum(y.out)
# }

## For all degrees
pdf(file = "./figures/all_deg_dist_pl_weekend_cl1.pdf")
op <- par(mfrow = c(2, 1))
options(scipen = 10)
## Histogram and cumulative distribution function
hist(degrees, freq = FALSE, xlab = "Degree k",
     main = "Histogram of All Degrees On Weekend Cluster 1", breaks = 50, col = "gray")
mainStr <- "Degree Power-law Distribution On Weekend Cluster 1"
plot(x, cumy, log = "xy", xlab = "Degree k",
     ylab = expression(Pr(x) >= k), cex = 0.5, main = mainStr)
## Overlay the fitted distribution
startval <- cumy[pl$xmin]
fittedvals <- (pl$xmin:max(x))^(-pl$alpha + 1) * (startval) / pl$xmin^(-pl$alpha + 1)
points(pl$xmin:max(x), fittedvals, type = 'l', col = 'red')
## Paste value of alpha onto the plot
alpha <- pl$alpha
text(80, .80, labels = bquote(paste(alpha, " = ", .(alpha))), col = "red", pos = 1)
dev.off()

# ## For indegrees and outdegrees
# pdf(file = "./figures/sna/in_out_deg_dist_pl.pdf")
# op <- par(mfrow = c(2, 2))
# options(scipen = 10)
# 
# ## Histogram and cumulative distribution function
# hist(indegrees, freq = FALSE, xlab = "Indegree k",
#      main = "Histogram of Indegrees", breaks = 50, col = "gray")
# mainStr.in <- "Indegree Power-law Distr."
# plot(x.in, cumy.in, log = "xy", xlab = "Indegree k",
#      ylab = expression(Pr(x) >= k), cex = 0.5, main = mainStr.in)
# 
# ## Overlay the fitted distribution
# startval.in <- cumy.in[pl.in$xmin]
# fittedvals.in <- (pl.in$xmin:max(x))^(-pl.in$alpha + 1) * (startval.in) / pl.in$xmin^(-pl.in$alpha + 1)
# points(pl.in$xmin:max(x), fittedvals.in, type = 'l', col = 'red')
# 
# ## Paste value of alpha onto the plot
# alpha.in <- pl.in$alpha
# text(60, .50, labels = bquote(paste(alpha, " = ", .(alpha.in))), col = "red", pos = 1)
# 
# ## Histogram and cumulative distribution function
# hist(outdegrees, freq = FALSE, xlab = "Outdegree k",
#      main = "Histogram of Outdegrees", breaks = 50, col = "gray")
# mainStr.out <- "Outdegree Power-law Distr."
# plot(x.out, cumy.out, log = "xy", xlab = "Outdegree k",
#      ylab = expression(Pr(x) >= k), cex = 0.5, main = mainStr.out)
# 
# ## Overlay the fitted distribution
# startval.out <- cumy.out[pl.out$xmin]
# fittedvals.out <- (pl.out$xmin:max(x))^(-pl.out$alpha + 1) * (startval.out) / pl.out$xmin^(-pl.out$alpha + 1)
# points(pl.out$xmin:max(x), fittedvals.out, type = 'l', col = 'red')
# 
# ## Paste value of alpha onto the plot
# alpha.out <- pl.out$alpha
# text(80, .80, labels = bquote(paste(alpha, " = ", .(alpha.out))), col = "red", pos = 1)
# 
# # ## Save the plots and reset to the previous settings
# dev.off()
par(op)

# create social graph for cluster #2

subset.call.cluster <- subset(call.cluster, cluster %in% 2)

subset.caller.data <- subset(call.data, caller_id %in% subset.call.cluster$call_num)
subset.callee.data <- subset(call.data, callee_id %in% subset.call.cluster$call_num)

my.cluster.call <- rbind(subset.caller.data,subset.callee.data)

my.call.data <- subset(my.cluster.call, date > 20080300) #change date
my.call.data <- subset(my.cluster.call, date < 20080303)

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
print(sizes(fc))

## Sample random colors for the communities
fc.colors <- sample(colors(), length(fc))
## Color the nodes by the community
for(i in 1:length(fc.colors)) {
  V(g)[membership(fc)==i]$color <- fc.colors[i]
}

## Size the vertices according to their outdegrees
V(g)$size <- degree(g, mode="all") / 2

## Finally, plot and save the network
filename <- "./figures/mobile_network_igraph_weekend_cluster2.pdf"
mainStr <- "Mobile Data Network Visualization on Weekend Cluster #2"
plotNetwork(g, mainStr, filename, width=15, height=15)

## Get Coeficient Clustering

my.cluster.call <- rbind(subset.caller.data,subset.callee.data)

my.call.data <- subset(my.cluster.call, date > 20080300) #change date
my.call.data <- subset(my.cluster.call, date < 20080303)

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

## Global clustering coefficient of the network
print("Global clustering coefficient of g:")
print(transitivity(g, type="global"))

## Network structure -- degree distributions: all degrees, indegree, and outdegree
degrees <- degree(g, mode = "all")
degrees <- subset(degrees, degrees > 0) # remove all zero degrees
indegrees <- degree(g, mode = "in")
indegrees <- subset(indegrees, indegrees > 0)
outdegrees <- degree(g, mode = "out")
outdegrees <- subset(outdegrees, outdegrees > 0)

## Fit the power law to 3 kinds of degree distribution
pl <- plfit(degrees)
# pl.in <- plfit(indegrees)
# pl.out <- plfit(outdegrees)
print("Overall degree distribution:")
print(pl) # test if it is power-lawed and print its alpha exponent
# print("Indegree distribution:")
# print(pl.in)
# print("Outdegree distribution:")
# print(pl.out)

## Plot the cumulative empirical distribution
cumy <- c() # empty, expandable vector
y <- tabulate(degrees)
x <- 1:length(y)
for(i in 1:length(x)) {
  cumy[i] <- sum(y[i:length(x)]) / sum(y)
}
# ## For indegrees
# cumy.in <- c()
# y.in <- tabulate(indegrees)
# x.in <- 1:length(y.in)
# for(i in 1:length(x.in)) {
#   cumy.in[i] <- sum(y.in[i:length(x.in)]) / sum(y.in)
# }
# ## For outdegrees
# cumy.out <- c()
# y.out <- tabulate(outdegrees)
# x.out <- 1:length(y.out)
# for(i in 1:length(x.out)) {
#   cumy.out[i] <- sum(y.out[i:length(x.out)]) / sum(y.out)
# }

## For all degrees
pdf(file = "./figures/all_deg_dist_pl_weekend_cl2.pdf")
op <- par(mfrow = c(2, 1))
options(scipen = 10)
## Histogram and cumulative distribution function
hist(degrees, freq = FALSE, xlab = "Degree k",
     main = "Histogram of All Degrees On Weekend Cluster 2", breaks = 50, col = "gray")
mainStr <- "Degree Power-law Distribution On Weekend Cluster 2"
plot(x, cumy, log = "xy", xlab = "Degree k",
     ylab = expression(Pr(x) >= k), cex = 0.5, main = mainStr)
## Overlay the fitted distribution
startval <- cumy[pl$xmin]
fittedvals <- (pl$xmin:max(x))^(-pl$alpha + 1) * (startval) / pl$xmin^(-pl$alpha + 1)
points(pl$xmin:max(x), fittedvals, type = 'l', col = 'red')
## Paste value of alpha onto the plot
alpha <- pl$alpha
text(80, .80, labels = bquote(paste(alpha, " = ", .(alpha))), col = "red", pos = 1)
dev.off()

# ## For indegrees and outdegrees
# pdf(file = "./figures/sna/in_out_deg_dist_pl.pdf")
# op <- par(mfrow = c(2, 2))
# options(scipen = 10)
# 
# ## Histogram and cumulative distribution function
# hist(indegrees, freq = FALSE, xlab = "Indegree k",
#      main = "Histogram of Indegrees", breaks = 50, col = "gray")
# mainStr.in <- "Indegree Power-law Distr."
# plot(x.in, cumy.in, log = "xy", xlab = "Indegree k",
#      ylab = expression(Pr(x) >= k), cex = 0.5, main = mainStr.in)
# 
# ## Overlay the fitted distribution
# startval.in <- cumy.in[pl.in$xmin]
# fittedvals.in <- (pl.in$xmin:max(x))^(-pl.in$alpha + 1) * (startval.in) / pl.in$xmin^(-pl.in$alpha + 1)
# points(pl.in$xmin:max(x), fittedvals.in, type = 'l', col = 'red')
# 
# ## Paste value of alpha onto the plot
# alpha.in <- pl.in$alpha
# text(60, .50, labels = bquote(paste(alpha, " = ", .(alpha.in))), col = "red", pos = 1)
# 
# ## Histogram and cumulative distribution function
# hist(outdegrees, freq = FALSE, xlab = "Outdegree k",
#      main = "Histogram of Outdegrees", breaks = 50, col = "gray")
# mainStr.out <- "Outdegree Power-law Distr."
# plot(x.out, cumy.out, log = "xy", xlab = "Outdegree k",
#      ylab = expression(Pr(x) >= k), cex = 0.5, main = mainStr.out)
# 
# ## Overlay the fitted distribution
# startval.out <- cumy.out[pl.out$xmin]
# fittedvals.out <- (pl.out$xmin:max(x))^(-pl.out$alpha + 1) * (startval.out) / pl.out$xmin^(-pl.out$alpha + 1)
# points(pl.out$xmin:max(x), fittedvals.out, type = 'l', col = 'red')
# 
# ## Paste value of alpha onto the plot
# alpha.out <- pl.out$alpha
# text(80, .80, labels = bquote(paste(alpha, " = ", .(alpha.out))), col = "red", pos = 1)
# 
# # ## Save the plots and reset to the previous settings
# dev.off()
par(op)