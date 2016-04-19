library(Information)
library(gridExtra)

options(scipen = 10)

### Loading the data
data(train, package = "Information")
data(valid, package = "Information")

### Exclude the control group
train <- subset(train, TREATMENT == 1)
valid <- subset(valid, TREATMENT == 1)

### Ranking variables using penalized IV  
IV <- create_infotables(data = train,
                   valid = valid,
                   y = "PURCHASE")

grid.table(head(IV$Summary), rows = NULL)

grid.table(IV$Tables$N_OPEN_REV_ACTS, rows = NULL)
SinglePlot(IV, "N_OPEN_REV_ACTS")

MultiPlot(IV, IV$Summary$Variable[1:9])

## Omit CV
IV <- create_infotables(data = train, y = "PURCHASE")
grid.table(head(IV$Summary), rows = NULL)

## Change bins count
IV <- create_infotables(data = train,
                   valid = valid,
                   y = "PURCHASE",
                   bins = 20)

grid.table(IV$Tables$N_OPEN_REV_ACTS,
           rows = NULL)

## Net IV example
data(train, package = "Information")
data(valid, package = "Information")

NIV <- create_infotables(data=train,
                    valid=valid,
                    y="PURCHASE",
                    trt="TREATMENT")

grid.table(head(NIV$Summary),
           rows=NULL)

MultiPlot(IV, NIV$Summary$Variable[1:9])

### Variable Clustering with IV analysis
library(ClustOfVar)
library(reshape2)
library(plyr)

data(train, package="Information")
data(valid, package="Information")
train <- subset(train, TREATMENT==1)
valid <- subset(valid, TREATMENT==1)

tree <- hclustvar(train[, !(names(train) %in% c("PURCHASE", "TREATMENT"))])
nvars <- length(tree[tree$height < 0.7])
part_init <- cutreevar(tree, nvars)$cluster

kmeans <- kmeansvar(X.quanti = train[, !(names(train) %in% c("PURCHASE", "TREATMENT"))], init = part_init)
clusters <- cbind.data.frame(melt(kmeans$cluster), row.names(melt(kmeans$cluster)))
names(clusters) <- c("Cluster", "Variable")
clusters <- join(clusters, IV$Summary, by = "Variable", type = "left")
clusters <- clusters[order(clusters$Cluster),]
# GROUP BY Cluster, then ORDER BY AdjIV DESC
clusters$Rank <- ave(-clusters$AdjIV, clusters$Cluster, FUN = rank)
selected_members <- subset(clusters, Rank == 1)
selected_members$Rank <- NULL
nrow(selected_members)
nrow(clusters)

grid.table(head(selected_members),
           rows=NULL)